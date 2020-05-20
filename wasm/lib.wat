(module $lib
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_read" (func $fd_read (param i32 i32 i32 i32) (result i32)))
  
  (memory 1)
  (export "memory" (memory 0))
  
  ;; 0-3 is reserved for a dump of size i32
  (data (i32.const 0) "\00\00\00\00")     ;; init with 0x00000000
  
  ;; 4-31 is reserved for i32 stdout buffer
  (data (i32.const 4) "\14")              ;; pointer to data: 20
  (data (i32.const 8) "\00")              ;; length: 0
  (data (i32.const 12) "\0C")             ;; capacity: 12 bytes
  (data (i32.const 16) "\01")             ;; stride: 1 byte
  (data (i32.const 20) "\00")             ;; data
  
  ;; 32-51 true
  (data (i32.const 32) "\30")             ;; pointer to data: 44
  (data (i32.const 36) "\04")             ;; length: 4
  (data (i32.const 40) "\04")             ;; capacity: 4 bytes
  (data (i32.const 44) "\01")             ;; stride: 1
  (data (i32.const 48) "true")            ;; data
  
  ;; 52-75 false
  (data (i32.const 52) "\44")             ;; pointer to data: 68
  (data (i32.const 56) "\05")             ;; length: 5
  (data (i32.const 60) "\08")             ;; capacity: 8 bytes
  (data (i32.const 64) "\01")             ;; stride: 1
  (data (i32.const 68) "false\00\00\00")  ;; data, pad with 3 bytes
  
  ;; store newline for "writeln"
  ;; can store more static data
  (data (i32.const 76) "\5C")             ;; pointer to data: 92
  (data (i32.const 80) "\01")             ;; length: 1
  (data (i32.const 84) "\04")             ;; capacity: 4
  (data (i32.const 88) "\01")             ;; stride: 1
  (data (i32.const 92) "\n\00\00\00")     ;; data
  
  (global $i32_dump i32 (i32.const 0))
  (global $newline_buffer i32 (i32.const 76))
  
  ;; use these offsets when saving byte arrays
  (global $offset_length i32 (i32.const 4))
  (global $offset_capacity i32 (i32.const 8))
  (global $offset_stride i32 (i32.const 12))
  
  ;; locations of the buffers for printing values
  (global $i32_stdout_buffer i32 (i32.const 4))
  (global $true_buffer i32 (i32.const 32))
  (global $false_buffer i32 (i32.const 52))
  
  ;; write an array of bytes starting at given loc to stdin followed by a newline character
  (func $writeln (export "writeln") (param $pmem i32)
    ;; assumptions:
    ;; * The pointer should point to consecutive 8 bytes, that contain
    ;;   the pointer to data and number of bytes
    i32.const 1
    local.get $pmem
    i32.const 1
    global.get $i32_dump
    call $fd_write
    drop
    
    call $print_newline)
  
  ;; write '\n' to stdout
  (func $print_newline (export "print_newline")
    i32.const 1
    global.get $newline_buffer
    i32.const 1
    global.get $i32_dump
    call $fd_write
    drop)
  
  ;; check if given integer is zero everywhere except LSB
  (func $is_one_or_zero (export "is_one_or_zero") (param i32) (result i32)
    local.get 0
    i32.const 0xfffffffe
    i32.and
    i32.eqz)
  
  ;; write given int to stdout
  (func $print_int (export "print_int") (param $value i32)
    ;; convert value to byte array
    local.get $value
    global.get $i32_stdout_buffer
    call $itoa
    
    global.get $i32_stdout_buffer
    call $reverse_bytes
    
    i32.const 1
    global.get $i32_stdout_buffer
    i32.const 1
    global.get $i32_dump
    call $fd_write
    drop)
   
  ;; write given bool to stdout
  (func $print_bool (export "print_bool") (param $value i32)
    (block
      (block
        local.get $value
        i32.eqz
        br_if 0
        i32.const 1
        global.get $true_buffer
        i32.const 1
        global.get $i32_dump
        call $fd_write
        drop
        br 1)
    
      i32.const 1
      global.get $false_buffer
      i32.const 1
      global.get $i32_dump
      call $fd_write
      drop))
  
  ;; convert an array of bytes to a boolean value, represented as i32
  (func $atob (export "atob") (param $pmem i32) (result i32)
    ;; assumptions:
    ;; * Only 0 is regarded as false, all other numbers are true, even negative ones
    ;; * Shis function does not try to handle text based values, i.e. it does not convert
    ;;   "true" or "false" strings to anything. Since it uses atoi, all non-numerical
    ;;   data will lead to garbage values, which in this case probably always yield true,
    ;;   since 0 is the only false value as per above.
    local.get $pmem
    call $atoi
    i32.eqz
    i32.eqz)
 
  ;; convert array of bytes to an i32 value
  (func $atoi (export "atoi") (param $pmem i32) (result i32)
    (local $pdata i32)
    (local $pend i32)
    (local $n i32)
    (local $i i32)
    (local $v i32)
    (local $sign i32)
    ;; assumptions:
    ;; * $pmem points to a location with 8 consecutive bytes s.t.
    ;;   the first 4 point to the data and the next 4 contain the number of bytes
    ;; * The data is actually numerical data. Byte strings that are not of the form
    ;;   [0x2D] (0x30-0x39)* will result in gargabe values
    ;; * traps if length is 0 or if length is 1 and contains only '-' character or 0x2D byte
    
    ;; setup data pointer and len
    local.get $pmem
    i32.load
    local.set $pdata
    local.get $pmem
    i32.const 4
    i32.add
    i32.load
    local.tee $n
    local.get $pdata
    i32.add
    local.set $pend
    
    i32.const 0
    local.set $i
    i32.const 0
    local.set $v
    i32.const 1
    local.set $sign
    
    ;; trap if length is 0
    (block
      local.get $n
      i32.eqz
      i32.eqz
      br_if 0
      unreachable)
    
    ;; check if negative
    (block
      local.get $pdata
      i32.load8_u
      i32.const 0x2D
      i32.eq
      i32.eqz
      br_if 0
      i32.const -1
      local.set $sign
      i32.const 1
      local.get $i
      i32.add
      local.set $i
      ;; if first char is '-' and length is 1, trap
      (block
        local.get $n
        i32.const 1
        i32.sub
        i32.eqz
        i32.eqz
        br_if 1
        unreachable))
    
    ;; i points to first digit
    local.get $i
    local.get $pdata
    i32.add
    local.set $i
    
    (block
      (loop
        ;; load next byte, subtract zero byte (0x30) from it to get the number
        ;; multiply old value with 10 and add the difference to it
        local.get $i
        i32.load8_u
        i32.const 0x30
        i32.sub
        local.get $v
        i32.const 10
        i32.mul
        i32.add
        local.set $v
    
        local.get $i
        i32.const 1
        i32.add
        local.tee $i
        local.get $pend
        i32.eq
        br_if 1
        br 0))
    
    local.get $v
    local.get $sign
    i32.mul)
  
  ;; convert integer to a byte representation, i.e. an array of bytes
  (func $itoa (export "itoa") (param $v i32) (param $pmem i32)
    (local $is_negative i32)
    (local $r i32)
    (local $i i32)
    (local $b i32)
    (local $pdata i32)
    ;; assumptions:
    ;; * $pmem is a pointer to memory s.t.
    ;;   first 4 bytes contain a pointer to data,
    ;;   next 4 bytes hold the number of bytes in data
    ;;   and last 4 bytes contain maximum capacity of data
    
    local.get $pmem
    i32.load
    local.set $pdata
    
    ;; set base to 10
    i32.const 10
    local.set $b
    
    ;; check if int is negative
    (block
      local.get $v
      i32.const 0x80000000
      i32.and
      i32.const 31
      i32.shr_u
      local.tee $is_negative
      i32.eqz
      br_if 0
      ;; swap number to positive before modulo calculations
      local.get $v
      i32.const 0x7fffffff
      i32.and
      i32.const 0x7fffffff
      i32.xor
      i32.const 1
      i32.add
      local.set $v)
    
    (block
      local.get $pdata
      local.set $i
      (loop
        local.get $v
        local.get $b
        i32.rem_u
        local.set $r
    
        local.get $i
        i32.const 0x30 ;; '0'
        local.get $r ;; add remainder, to get the byte for 0-9
        i32.add
        i32.store8
    
        ;; i = i + 1
        local.get $i
        i32.const 1
        i32.add
        local.set $i
    
        ;; v = v - r
        local.get $v
        local.get $r
        i32.sub
        local.tee $v
        i32.eqz ;; break, if v - r = 0
        br_if 1
    
        ;; v = v / b
        local.get $v
        local.get $b
        i32.div_u
        local.set $v
        br 0))
    (block
      local.get $is_negative
      i32.eqz
      br_if 0
      local.get $i
      i32.const 0x2D ;; add '-' byte
      i32.store8
      local.get $i
      i32.const 1
      i32.add
      local.set $i)
    
    ;; store length
    local.get $pmem
    global.get $offset_length
    i32.add
    local.get $i
    local.get $pdata
    i32.sub
    i32.store)
 
  ;; reverse bytes in place.
  (func $reverse_bytes (export "reverse_bytes")  (param $pmem i32)
    (local $i i32)
    (local $j i32)
    (local $n i32)
    (local $n2 i32)
    (local $t i32)
    (local $pdata i32)
    ;; assumptions:
    ;; * $pmem contains 8 bytes of data, with first 4 bytes pointing
    ;;   to location of data and next 4 bytes being the number of bytes at that loc
    ;; * works for strings, if string consists of one byte characters
    
    local.get $pmem
    i32.load
    local.set $pdata
    local.get $pmem
    i32.const 4
    i32.add
    i32.load
    local.set $n
    
    (block
      ;; byte strings of lenght 0 and 1 are their own reverses, early exit
      local.get $n
      call $is_one_or_zero
      br_if 0
    
      ;; i points to first byte 
      local.get $pdata
      local.set $i
    
      ;; n2 points to midpoint, i.e. floor(n/2) + pmem
      local.get $n
      i32.const 1
      i32.shr_u
      local.get $pdata
      i32.add
      local.set $n2
    
      ;; j points to last byte
      local.get $n
      local.get $pdata
      i32.add
      i32.const 1
      i32.sub
      local.set $j
      (block 
        (loop
          ;; temp = arr[i]
          local.get $i
          i32.load8_u
          local.set $t 
    
          ;; arr[i] = arr[j]
          local.get $i
          local.get $j
          i32.load8_u
          i32.store8
    
          ;; arr[j] = temp = arr[i]
          local.get $j
          local.get $t
          i32.store8
    
          ;; i = i + 1
          i32.const 1
          local.get $i
          i32.add
          local.tee $i
          local.get $n2
          i32.eq
          br_if 1 ;; break, if i == floor(n/2)
    
          ;; j = j - 1
          local.get $j
          i32.const 1
          i32.sub
          local.set $j
          br 0))))

  (func $allocate (export "allocate") (param $n_bytes i32) (result i32)
    ;; allocates n_bytes from linear memory
    ;; and returns the address
    i32.const 0)

  (func $check_bounds (export "check_bounds") (param $addr i32) (param $idx i32) (param $str_idx i32) (result i32)
    ;; checks that the length of array at $addr >= $idx
    ;; returns $idx if yes, otherwise prints the literal string at $str_idx and becomes unreachable

    ;; assumptions:
    ;; - addr points to a location that contains 4 32 bit values:
    ;;   - 0 = pointer to data
    ;;   - 1 = length
    ;;   - 2 = capacity
    ;;   - 3 = stride, i.e. how many bytes one value takes
    ;; - idx is in units of stride, not of bytes,
    ;;   i.e. i = 1 -> bytes 4-7
    (block
      (block
        local.get $addr
        global.get $offset_length
        i32.add
        i32.load
        local.get $idx
        local.get $addr
        global.get $offset_stride
        i32.add
        i32.load
        i32.mul
        i32.gt_s
        br_if 1)
      local.get $str_idx
      call $get_string_literal
      call $write_string
      unreachable)
    local.get $idx)

  (func $new_array (export "new_array") (param $stride i32) (result i32)
    ;; allocates space for four i32 values: pdata, length, capacity, stride
    ;; allocates 1024 bytes of memory at "pdata" (use "allocate")
    ;; returns a pointer to the first of the four consecutive i32 values
    i32.const 0)

  (func $copy_array (export "copy_array") (param $dst i32) (param $src i32) (result i32)
    ;; copies values of length and capacity from src to dst
    ;; copies bytes from src pdata to dst pdata
    ;; returns the value dst
    i32.const 0)
  
  (func $get_string_literal (export "get_string_literal") (param $idx i32) (result i32)
    ;; gets the index of a stored string literal
    ;; calculates/fetches the address from that index
    ;; returns the address of the string literal
    i32.const 0)
 
  (func $read_input (export "read_input")
    ;; reads the contents of stdin to the stdin array (pretty much fd_read)
    )
  
  (func $bool_from_input (export "bool_from_input") (result i32)
    ;; converts the next whitespaced delimited value from stdin buffer to a bool(i32, [0, 1]) value (atob)
    ;; returns the converted value
    i32.const 0)
  
  (func $i32_from_input (export "i32_from_input")  (result i32)
    ;; converts the next whitespaced delimited value from stdin buffer to a i32 value (atoi)
    ;; returns the converted value
    i32.const 0)
  
  (func $f32_from_input (export "f32_from_input") (result f32)
    ;; converts the next whitespaced delimited value from stdin buffer to a f32 value (atof)
    ;; returns the converted value
    f32.const 0)
  
  (func $string_from_input (export "string_from_input") (result i32)
    ;; creates a new string (call new_array)
    ;; adds the next whitespace delimited value from stdin buffer to that string
    ;; returns the address of the string
    i32.const 0)
  
  (func $write_bool (export "write_bool") (param $value i32) 
    ;; takes in a boolean value and converts it to "true"/"false" string and prints it to stdout (btoa)
    )
  
  (func $write_i32 (export "write_i32") (param $value i32) 
    ;; takes in a i32 value and converts it to a byte string (itoa) and prints it to stdout
    )
  
  (func $write_f32 (export "write_f32") (param $value f32) 
    ;; takes in a f32 value and converts it to a byte string (ftoa) and prints it to stdout
    )
  
  (func $write_string (export "write_string") (param $addr i32) 
    ;; writes the contents of the string at "addr" to stdout
    )
  
  (func $string_eq (export "string_eq") (param $addr1 i32) (param $addr2 i32) (result i32)
    ;; takes the addresses of two strings and compares their lengths
    ;; returns 1 if equal 0 if not
    i32.const 0)
  
  (func $string_neq (export "string_neq") (param $addr1 i32) (param $addr2 i32) (result i32)
    ;; takes the addresses of two strings and compares their lengths
    ;; returns 0 if equal, 1 if not
    i32.const 0)
 
  (func $string_great (export "string_great") (param $addr1 i32) (param $addr2 i32) (result i32)
    ;; takes the addresses of two strings and compares their lengths
    ;; returns 1 if str1.len > str2.len, 0 otherwise
    i32.const 0)
  
  (func $string_great_eq (export "string_great_eq") (param $addr1 i32) (param $addr2 i32) (result i32)
    ;; takes the addresses of two strings and compares their lengths
    ;; returns 1 if str1.len >= str2.len, 0 otherwise
    i32.const 0)
  
  (func $string_less (export "string_less") (param $addr1 i32) (param $addr2 i32) (result i32)
    ;; takes the addresses of two strings and compares their lengths
    ;; returns 1 if str1.len < str2.len, 0 otherwise
    i32.const 0)
  
  (func $string_less_eq (export "string_less_eq") (param $addr1 i32) (param $addr2 i32) (result i32)
    ;; takes the addresses of two strings and compares their lengths
    ;; returns 1 if str1.len <= str2.len, 0 otherwise
    i32.const 0)
  
  (func $string_concatenate (export "string_concatenate") (param $addr1 i32) (param $addr2 i32) (result i32)
    ;; takes as input two strings
    ;; adds the contents of the second to the first at "addr1 + str1.len", while within capacity
    ;; returns "addr1"
    i32.const 0)
  
  (func $array_size (export "array_size") (param $addr i32) (result i32)
    ;; returns the length of the array at addr divided by stride
    ;; assumptions:
    ;; - addr points to a location that contains 4 32 bit values:
    ;;   - 0 = pointer to data
    ;;   - 1 = length
    ;;   - 2 = capacity
    ;;   - 3 = stride, i.e. how many bytes one value takes
    ;; - idx is in units of stride, not of bytes,
    ;;   i.e. i = 1 -> bytes 4-7
    local.get $addr
    global.get $offset_length
    i32.add
    i32.load
    local.get $addr
    global.get $offset_stride
    i32.add
    i32.load
    i32.div_s)
  
  (func $array_access_i (export "array_access_i") (param $addr i32) (param $idx i32) (param $str_idx i32)  (result i32)
    ;; returns the value at addr + idx * stride
    ;; assumptions:
    ;; - addr points to a location that contains 4 32 bit values:
    ;;   - 0 = pointer to data
    ;;   - 1 = length
    ;;   - 2 = capacity
    ;;   - 3 = stride, i.e. how many bytes one value takes
    ;; - idx is in units of stride, not of bytes,
    ;;   i.e. i = 1 -> bytes 4-7
    local.get $addr
    local.get $idx
    local.get $str_idx
    call $check_bounds 
    local.get $addr
    global.get $offset_stride
    i32.add
    i32.load
    i32.mul
    i32.load)
  
  (func $array_access_f (export "array_access_f") (param $addr i32) (param $idx i32) (param $str_idx i32)  (result f32)
    ;; returns the value at addr + idx * stride
    ;; assumptions:
    ;; - addr points to a location that contains 4 32 bit values:
    ;;   - 0 = pointer to data
    ;;   - 1 = length
    ;;   - 2 = capacity
    ;;   - 3 = stride, i.e. how many bytes one value takes
    ;; - idx is in units of stride, not of bytes,
    ;;   i.e. i = 1 -> bytes 4-7
    local.get $addr
    local.get $idx
    local.get $str_idx
    call $check_bounds
    local.get $addr
    global.get $offset_stride
    i32.add
    i32.load
    i32.mul
    f32.load)
  
  (func $array_assign_i (export "array_assign_i") (param $addr i32) (param $idx i32) (param $str_idx i32) (param $value i32) 
    ;; store value to addr + idx * stride
    ;; assumptions:
    ;; - addr points to a location that contains 4 32 bit values:
    ;;   - 0 = pointer to data
    ;;   - 1 = length
    ;;   - 2 = capacity
    ;;   - 3 = stride, i.e. how many bytes one value takes
    ;; - idx is in units of stride, not of bytes,
    ;;   i.e. i = 1 -> bytes 4-7
    local.get $addr
    local.get $idx
    local.get $str_idx
    call $check_bounds 
    local.get $addr
    global.get $offset_stride
    i32.add
    i32.load
    i32.mul
    local.get $value
    i32.store)
  
  (func $array_assign_f (export "array_assign_f") (param $addr i32) (param $idx i32) (param $str_idx i32) (param $value f32) 
    ;; store value to addr + idx * stride
    ;; assumptions:
    ;; - addr points to a location that contains 4 32 bit values:
    ;;   - 0 = pointer to data
    ;;   - 1 = length
    ;;   - 2 = capacity
    ;;   - 3 = stride, i.e. how many bytes one value takes
    ;; - idx is in units of stride, not of bytes,
    ;;   i.e. i = 1 -> bytes 4-7
    local.get $addr
    local.get $idx
    local.get $str_idx
    call $check_bounds 
    local.get $addr
    global.get $offset_stride
    i32.add
    i32.load
    i32.mul
    local.get $value
    f32.store)
  )
