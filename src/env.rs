#[derive(Debug, PartialEq)]
pub struct Environment<'a> {
    pub env: std::collections::HashMap<String, crate::obj::Object>,
    pub enclosing: Option<&'a mut Environment<'a>>,
}

impl<'a> Default for Environment<'a>{
    fn default() -> Self {
        let env = std::collections::HashMap::new();
        // add some default properties here, or provide an interface
        Environment{ env, enclosing: None }
    }
}

impl<'a> Environment<'a> {
    pub fn new() -> Environment<'a> {
        let env = std::collections::HashMap::new();
        Environment{ env, enclosing: None }
    }

    pub fn from(enclosing: &'a mut Environment<'a>) -> Environment<'a> {
        Environment {
            env: std::collections::HashMap::new(),
            enclosing: Some(enclosing)
        }
    }

    pub fn get(&mut self, what: &String) -> Option<&crate::obj::Object> {
        if self.env.contains_key(what) {
            return self.env.get(what);
        } else {
            match &mut self.enclosing {
                None => { return None; }
                Some(s) => {
                    return s.get(what);
                }
            };
        }
    }

    pub fn set(&mut self, name: String, value: crate::obj::Object) {
        if self.env.contains_key(&name) {
            self.env.insert(name, value);
        } else {
            match &mut self.enclosing {
                None => { return; }
                Some(e) => {
                    e.set(name, value);
                }
            };
        }
    }
}
