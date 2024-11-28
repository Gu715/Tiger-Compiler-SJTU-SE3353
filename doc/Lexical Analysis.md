# Lab2: Lexical Analysis

## 1 Handling Comments
### 1.1 Start of Comment
- When the scanner encounters the pattern `/*`, it recognizes the start of a comment, adjusts the character position, sets variable `comment_level_` to 1 (note that this variable is not initalized), and transits to `COMMENT` state.

### 1.2 Inside Comment
- If the scanner encounters the pattern `/*` again, indicating that there is a nested comment inside the comment, it increments variable `comment_level_`.
- If the scanner encouters the pattern `*/`, it decrements variable `comment_level_`. If `comment_level_` equals to 0, it signifies the end of comment, therefore the scanner transits to `INITIAL` state.
- Any other character inside the comment is ignored. The scanner calls fucntion `errormsg_->Newline()` when it encounters `\n` inside the comment.

### 1.3 Error Handling
- If the scanner encounters the end of file `EOF` while still in `COMMENT` state, it triggers an error indicating unterminated comment.


## 2 Handling Strings
### 2.1 Start of String
- When the scanner encounters the pattern `"`, it recognizes the start of a string, adjusts the character position, and transits to `STR` state.

### 2.2 Inside String
- If the scanner encounters an escape sequence, it has to convert the sequence to the real character. For example, `\n` is converted to the real linebreak character, and `\ddd` to the character whose ascii code is `ddd`.
- The escape sequence `\f___f\` is ignored, where `f___f` stands for a sequence of one or more formatting characters (a subset of the non-printable characters including at least space, tab, newline, formfeed). In this pattern, the scanner should traverse the sequence to handle all newline characters, and adjust the character position to the first position after the sequence.
- If the scanner encounters the pattern `"` again, it signifies the end of the string. The scanner sets the matched value with buffer `string_buf_`, clears the buffer and transits to `INITIAL` state.
- Any valid character inside the string should be placed into `string_buf_`.

### 2.3 Error Handling
- Inside the string, any escape sequence that is not recognized by any pattern is considered illegal use of `\`, triggering an error.
- In pattern `\ddd`, if the ascii code `ddd` is greater than 255 (the maximum ascii code), it is considered illegal ascii code, triggering an error.
- If the scanner encounters the end of file `EOF` while still in `STR` state, it triggers an error indicating unterminated string.


## 3 Error Handling
- Any token that is not recognized by any pattern is considered illegal, triggering an error.
- Refer to section 1.3 for handling unterminated comment.
- Refer to section 2.3 for handling unterminated string and illegal use of `\`.

## 4 End-of-File Handling
- When the scanner encounters EOF while still in `COMMENT` or `STR` state, it triggers an error. See sections 1.3 and 2.3.