use std::collections::BTreeMap;

use serde_json::Value;
use std::ops::Deref;

/// The JsonType enum represents the type structure of json data.
/// It is first used for deriving the structure of a json blob.
/// Then, it is used for generating Rust types.
#[derive(PartialEq, Hash, Clone, Debug)]
pub enum JsonType {
    /// Undetermined type. Used when a 'null' JSON value is encountered.
    Null,
    Bool,
    Number,
    String,
    /// URL type. Note that this is effectively a String, specialized to a URL.
    Url,
    Array(Box<JsonType>),
    /// A value that can be both null and an actual value.
    Object(ObjectType),
    Optional(Box<JsonType>),
    /// Represents a polymorphic type.
    Multi(Vec<JsonType>),
    /// A reference to a named object. Only useful with the TypeRegistry.
    ObjectReference(String),
}

/// Represents a typed JSON object with fields.
#[derive(PartialEq, Hash, Clone, Debug)]
pub struct ObjectType {
    pub fields: Vec<(String, JsonType)>,
}

impl ObjectType {
    /// Construct a new default object type without fields.
    pub fn new() -> Self {
        ObjectType { fields: Vec::new() }
    }

    /// Retrieve a field.
    pub fn field(&self, name: &str) -> Option<&JsonType> {
        self.fields
            .iter()
            .find(|&&(ref x, _)| x == name)
            .map(|&(_, ref t)| t)
    }

    /// Get a field as mutable.
    fn field_mut(&mut self, name: &str) -> Option<&mut JsonType> {
        self.fields
            .iter_mut()
            .find(|&&mut (ref x, _)| x == name)
            .map(|&mut (_, ref mut t)| t)
    }

    /// Get the index of a field with the given name.
    fn field_index(&self, name: &str) -> Option<usize> {
        self.fields.iter().position(|&(ref x, _)| x == name)
    }

    /// Refine the type of a field with another type.
    fn refine_field(&mut self, name: &str, typ: &JsonType) {
        if let Some(index) = self.field_index(name) {
            let t = self.fields[index].1.clone();
            self.fields[index] = (name.to_string(), t.refine(typ));
        } else {
            self.fields.push((name.to_string(), typ.clone()));
            self.fields.sort_by(|&(ref a, _), &(ref b, _)| a.cmp(b));
        }
    }

    /// Refine all fields with the field types in the given object.
    fn refine(&self, b: &ObjectType) -> ObjectType {
        let mut a = self.clone();
        for &(ref name, ref typ) in b.fields.iter() {
            a.refine_field(name, typ);
        }
        a
    }
}

impl ::std::iter::FromIterator<(String, JsonType)> for ObjectType {
    fn from_iter<T: IntoIterator<Item = (String, JsonType)>>(iter: T) -> Self {
        let mut items: Vec<(String, JsonType)> = iter.into_iter().collect();
        items.sort_by(|&(ref a, _), &(ref b, _)| a.cmp(b));
        ObjectType { fields: items }
    }
}

impl JsonType {
    pub fn optional(t: JsonType) -> Self {
        JsonType::Optional(Box::new(t))
    }

    pub fn array(t: JsonType) -> Self {
        JsonType::Array(Box::new(t))
    }

    pub fn object(fields: ObjectType) -> Self {
        JsonType::Object(fields)
    }

    pub fn multi(types: Vec<JsonType>) -> Self {
        JsonType::Multi(types)
    }

    pub fn is_null(&self) -> bool {
        match self {
            &JsonType::Null => true,
            _ => false,
        }
    }

    pub fn is_bool(&self) -> bool {
        match self {
            &JsonType::Bool => true,
            _ => false,
        }
    }

    pub fn is_number(&self) -> bool {
        match self {
            &JsonType::Number => true,
            _ => false,
        }
    }

    pub fn is_string(&self) -> bool {
        match self {
            &JsonType::String => true,
            _ => false,
        }
    }

    pub fn is_array(&self) -> bool {
        match self {
            &JsonType::Array(_) => true,
            _ => false,
        }
    }

    pub fn is_array_of(&self, typ: &JsonType) -> bool {
        match self {
            &JsonType::Array(ref t) => t.deref() == typ,
            _ => false,
        }
    }

    pub fn is_object(&self) -> bool {
        match self {
            &JsonType::Object(_) => true,
            _ => false,
        }
    }

    pub fn as_object(&self) -> Option<&ObjectType> {
        match self {
            &JsonType::Object(ref t) => Some(t),
            _ => None,
        }
    }

    pub fn is_object_of(&self, typ: &ObjectType) -> bool {
        match self {
            &JsonType::Object(ref t) => t == typ,
            _ => false,
        }
    }

    pub fn is_optional(&self) -> bool {
        match self {
            &JsonType::Optional(_) => true,
            _ => false,
        }
    }

    pub fn is_optional_of(&self, typ: &JsonType) -> bool {
        match self {
            &JsonType::Optional(ref t) => t.deref() == typ,
            _ => false,
        }
    }

    pub fn is_multi(&self) -> bool {
        match self {
            &JsonType::Multi(_) => true,
            _ => false,
        }
    }

    pub fn is_multi_of(&self, other: &Vec<JsonType>) -> bool {
        match self {
            &JsonType::Multi(ref items) => items == other,
            _ => false,
        }
    }

    /// Refine a JsonType by analysing an additional type with the same structure.
    /// The function will make the JsonType more generic where necessary.
    pub fn refine(&self, other: &JsonType) -> JsonType {
        use self::JsonType::*;

        match (self, other) {
            (a, b) if a == b => {
                // Types are equal, so just re-use self.
                a.clone()
            }
            (&String, &Url) | (&Url, &String) => String,
            (&Optional(ref a), &Null) => {
                // Left is optional, right is null, so just re-use the right
                // type.
                Optional(a.clone())
            }
            (&Null, &Optional(ref a)) => {
                // Same as above, just in reverse.
                Optional(a.clone())
            }
            (&Optional(ref a), &Optional(ref b)) => Optional(Box::new(a.refine(b))),
            (&Optional(ref a), b) => {
                if a.deref() == b {
                    Optional(a.clone())
                } else {
                    JsonType::multi(vec![JsonType::Optional(a.clone()), b.clone()])
                }
            }
            (b, &Optional(ref a)) => {
                if a.deref() == b {
                    Optional(a.clone())
                } else {
                    JsonType::multi(vec![Optional(a.clone()), b.clone()])
                }
            }
            (&Null, b) => {
                // B must be non-null because of eq check above.
                // Assume type to be Option<B>.
                Optional(Box::new(b.clone()))
            }
            (a, &Null) => {
                // Same as above, just in reverse.
                Optional(Box::new(a.clone()))
            }
            (&Array(ref a_type), &Array(ref b_type)) => {
                // Both types are arrays, so merge item type.
                Array(Box::new(a_type.refine(b_type)))
            }
            (&Object(ref a_fields), &Object(ref b_fields)) => Object(a_fields.refine(b_fields)),
            (&Multi(ref a_types), &Multi(ref b_types)) => {
                let mut types = a_types.clone();
                for t in b_types.iter() {
                    if !types.contains(t) {
                        types.push(t.clone());
                    }
                }
                Multi(types)
            }
            (a, b) => Multi(vec![a.clone(), b.clone()]),
        }
    }
}

/// Returns true if the given value is a reserved word in RUST.
fn is_reserved_word(word: &str) -> bool {
    // FIXME: add all reserved words.
    match word {
        "type" => true,
        _ => false,
    }
}

/// Analyze a JSON value and build a representative JsonType.
pub fn analyse_value(value: &Value) -> JsonType {
    use serde_json::Value as V;
    use self::JsonType as T;
    match value {
        &V::Null => T::Null,
        &V::Bool(_) => T::Bool,
        &V::Number(_) => T::Number,
        &V::String(ref text) => {
            // Check for URL.
            let is_url = ::url::Url::parse(text).is_ok();
            if is_url {
                T::Url
            } else {
                T::String
            }
        }
        &V::Array(ref values) => {
            let typ = if values.len() < 1 {
                T::Null
            } else {
                values[1..]
                    .iter()
                    .map(analyse_value)
                    .fold(analyse_value(&values[0]), |a, b| a.refine(&b))
            };
            JsonType::Array(Box::new(typ))
        }
        &V::Object(ref map) => T::Object(
            map.iter()
                .map(|(key, value)| {
                    let name = if is_reserved_word(&key) {
                        format!("_{}", key)
                    } else {
                        key.to_string()
                    };

                    (name, analyse_value(value))
                })
                .collect(),
        ),
    }
}

pub struct TypeRegistry {
    pub types: BTreeMap<String, JsonType>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        TypeRegistry {
            types: BTreeMap::new(),
        }
    }

    fn normalize(&mut self, typ: JsonType) -> JsonType {
        use self::JsonType as T;
        match typ {
            T::Array(t) => T::array(self.normalize(*t)),
            T::Object(obj) => T::object(
                obj.fields
                    .into_iter()
                    .map(|(name, t)| (name, self.normalize(t)))
                    .collect(),
            ),
            T::Optional(t) => T::optional(self.normalize(*t)),
            _ => typ,
        }
    }

    pub fn add(&mut self, name: String, typ: JsonType, refine: bool) -> bool {
        let typ = self.normalize(typ);

        if let Some(t) = self.types.get(&name).map(|x| x.clone()) {
            if refine {
                self.types.insert(name, t.refine(&typ));
                true
            } else {
                self.types.insert(name, typ);
                true
            }
        } else {
            self.types.insert(name, typ);
            false
        }
    }
}

mod test {
    use super::*;

    #[test]
    fn test_refine_simple() {
        use self::JsonType::*;
        use self::JsonType as T;

        assert_eq!(Null, Null.refine(&Null));
        assert_eq!(Bool, Bool.refine(&Bool));
        assert_eq!(Number, Number.refine(&Number));
        assert_eq!(String, String.refine(&String));
        assert_eq!(
            T::optional(Bool),
            T::optional(Bool).refine(&T::optional(Bool))
        );
    }

    #[test]
    fn test_refine_optional() {
        use self::JsonType::*;
        use self::JsonType as T;

        assert_eq!(T::optional(Bool), Null.refine(&Bool));
        assert_eq!(T::optional(Bool), Bool.refine(&Null));
    }

    #[test]
    fn test_refine_array() {
        use self::JsonType::*;
        use self::JsonType as T;

        assert_eq!(
            T::array(T::optional(Bool)),
            T::array(Null).refine(&T::array(Bool))
        );
    }

    #[test]
    fn test_refine_object() {
        use self::JsonType::*;
        use self::JsonType as T;

        let f1 = JsonType::object(
            vec![("a".to_string(), Bool), ("b".to_string(), Null)]
                .into_iter()
                .collect(),
        );

        let f2 = JsonType::object(
            vec![("b".to_string(), Bool), ("c".to_string(), String)]
                .into_iter()
                .collect(),
        );

        let f3 = JsonType::object(
            vec![
                ("a".to_string(), Bool),
                ("b".to_string(), T::optional(Bool)),
                ("c".to_string(), String),
            ].into_iter()
                .collect(),
        );

        assert_eq!(f3, f1.refine(&f2));
    }

    #[test]
    fn test_refine_multi() {
        use self::JsonType::*;
        use self::JsonType as T;

        assert_eq!(T::multi(vec![Bool, String]), Bool.refine(&String));
    }

    #[test]
    fn test_analyse() {
        use self::JsonType::*;
        use self::JsonType as T;

        let data1 = json!({
            "boolean": true,
            "number": 22,
            "string": "abc",
            "opt": null,
            "array": [null, true, false],
        });

        let data2 = json!({
            "number": 22,
            "string": "abc",
            "opt": false,
            "array": [null, true, false],
            "object": &data1,
        });

        let t1 = analyse_value(&data1);
        let t2 = analyse_value(&data2);

        assert_eq!(
            T::object(
                vec![
                    ("boolean".to_string(), Bool),
                    ("number".to_string(), Number),
                    ("string".to_string(), String),
                    ("opt".to_string(), Null),
                    ("array".to_string(), T::array(T::optional(Bool))),
                ].into_iter()
                    .collect()
            ),
            t1
        );

        let t3 = t1.refine(&t2);
        assert_eq!(
            T::object(
                vec![
                    ("boolean".to_string(), Bool),
                    ("number".to_string(), Number),
                    ("string".to_string(), String),
                    ("opt".to_string(), T::optional(Bool)),
                    ("array".to_string(), T::array(T::optional(Bool))),
                    ("object".to_string(), t1.clone()),
                ].into_iter()
                    .collect()
            ),
            t3
        );
    }
}
