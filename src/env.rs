#[derive(Debug, Default, Clone)]
pub struct Environment {
    pub env: std::collections::HashMap<String, crate::obj::Object>
}

impl Environment {
    pub fn get(&mut self, what: &String) -> Option<&crate::obj::Object> {
        self.env.get(what)
    }
}
