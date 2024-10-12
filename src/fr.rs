use std::io::Read;

pub fn read_file(path: &std::path::Path) -> Vec<u8> {
    let mut buff = Vec::new();
    if !path.exists() {
        panic!("file {:?} does not exist", path);
    }
    let mut ff = std::fs::File::options()
        .read(true)
        .append(false)
        .write(false)
        .create(false)
        .create_new(false).open(path).unwrap();

    ff.read_to_end(&mut buff).unwrap();
    buff.push(0); // just in case
    buff
}
