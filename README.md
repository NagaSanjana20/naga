### 1. How to compile and run - 
#### Rust and Cargo are installed and they are checked whether they are sucessfully installed are or not by using these functions "rustc --version" and "cargo --version".
#### Then a new file is created with name "sql_project" and "src/main.rs" is created in it.
#### The code is then wrote in the "VS code" in main.rs and saved. the dependencies are also saved in cargo.toml file.
#### Then again come to ubuntu, compile and run the code using "cargo build and cargo run". It shows to enter the query, enter it and check whether the query is correct or not.

### 2. Listing the dependencies -
#### --> "sqlparser", version: 0.41.0, this helps to read the SQL query type and breaks into parts which helps the rust to understand the program
#### --> "maplit", version: 1.0.2, it helps us to create Hashmaps easily in rust and also to write tables using key-value pairs. 
#### --> "serde_json", version: 1., this helps to convert rust to JSON and JSON ot rust easily, particularly in this project it helps the query results to print in clear, readable JSON-like format.
### These all the dependencies are all included in "Cargo.toml"

### 3. Sample test case and output
#### The test case are run using "cargo test". 
#### For Example- 
#[test]

fn test_valid_queries() {

    assert!(query_is_correct("SELECT name FROM student WHERE name = 'Alice'"));
    
}
#### This helps in checking the query returns the expected result. 

#[test]

fn test_invalid_queries() {

    assert!(!query_is_correct("SELECT name student WHERE major = 'CS'"));
    
}
#### This checks that Query is incorrect and is rejected.
The output it showed is this:
running 2 tests
test tests::test_invalid_queries ... ok
test tests::test_valid_queries ... ok

test result: ok. 2 passed; 0 failed
This means that both the tests are passed and tells that my SQL is working as expected.


![Screenshot 2025-04-30 183903](https://github.com/user-attachments/assets/a7e42b11-d705-4334-8393-df32d0af9357)
![Screenshot 2025-04-30 183922](https://github.com/user-attachments/assets/77bf4861-f95d-41df-bfa8-134766a60510)
![Screenshot 2025-04-30 184645](https://github.com/user-attachments/assets/9f3ef568-5e6e-4cce-b111-0464ae55358b)
