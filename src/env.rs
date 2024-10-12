#[derive(Debug, Default)]
pub struct Environment {
    pub env: std::collections::HashMap<String, crate::obj::Object>
}
