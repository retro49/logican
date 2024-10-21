use crate::obj::Object;

pub struct Environment {
    pub env: std::collections::HashMap<String, crate::obj::Object>,
    pub enclosing: *mut Environment,
}

impl std::default::Default for Environment {
    fn default() -> Self {
        Environment{
            env: std::collections::HashMap::new(),
            enclosing: std::ptr::null_mut(),
        }
    }
}

impl Environment {
    pub fn contains(&self, key: &String) -> bool {
        self.env.contains_key(key)
    }

    pub fn contains_enclose(&self, key: &String) -> bool {
        if self.contains(key) {
            return true;
        }
        return false;
    }

    pub fn from(enclosing: *mut Environment) -> Environment {
        Environment {
            env: std::collections::HashMap::new(),
            enclosing
        }
    }

    pub fn set(&mut self, key: String, obj: Object) {
        self.env.insert(key, obj);
    }

    pub fn get(&mut self, key: &String) -> Option<&Object> {
        if self.contains(key) { 
            return self.env.get(key);
        }

        if self.enclosing == std::ptr::null_mut() {
            return None;
        }

        unsafe {
            let r = self.enclosing.as_mut().unwrap().get(key);
            return r;
        }
    }
}
