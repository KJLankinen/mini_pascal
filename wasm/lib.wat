(module $lib
  (import "wasi_snapshot_preview1" "fd_write" (func $fd_write (param i32 i32 i32 i32) (result i32)))
  (import "wasi_snapshot_preview1" "fd_read" (func $fd_read (param i32 i32 i32 i32) (result i32)))
  
  (memory 1)
  (export "memory" (memory 0))
  (export "dyn_mem_ptr" (global $dyn_mem_ptr))
  (export "static_str_ptrs" (global $static_str_ptrs))
  (export "offset_length" (global $offset_length))

  ;; -----------------------------------------------------------------
  ;; NOTE!
  ;; Byte order in data section is little endian
  ;; ----------------------------------------------------------------- 
  ;; 0-1023 is reserved for static data
  (data (i32.const 0) "\n> truefalse")

  ;; 1024-2047 is reserved for stdin buffer
  (data (i32.const 1024) "\10\04")          ;; pointer to data: 1040 = 0x0410
  (data (i32.const 1028) "\00")             ;; length: 0
  (data (i32.const 1032) "\F0\03")          ;; capacity: 1008 bytes
  (data (i32.const 1036) "\01")             ;; stride: 1 byte
  (data (i32.const 1040) "\00")             ;; data

  ;; 2048-2051 is reserved for a dump of size i32
  (data (i32.const 2048) "\00\00\00\00")    ;; init with 0x00000000
  
  ;; 2052-2079 is reserved for i32 stdout buffer
  (data (i32.const 2052) "\14\08")          ;; pointer to data: 0x0814, 2068
  (data (i32.const 2056) "\00")             ;; length: 0
  (data (i32.const 2060) "\0C")             ;; capacity: 12 bytes
  (data (i32.const 2064) "\01")             ;; stride: 1 byte
  (data (i32.const 2068) "\00")             ;; data
  
  ;; 2080-2095 true
  (data (i32.const 2080) "\03")             ;; pointer to data: 3
  (data (i32.const 2084) "\04")             ;; length: 4
  (data (i32.const 2088) "\04")             ;; capacity: 4 bytes
  (data (i32.const 2092) "\01")             ;; stride: 1
  
  ;; 2096-2111 false
  (data (i32.const 2096) "\07")             ;; pointer to data: 7
  (data (i32.const 2100) "\05")             ;; length: 5
  (data (i32.const 2104) "\05")             ;; capacity: 5 bytes
  (data (i32.const 2108) "\01")             ;; stride: 1
  
  ;; 2112-2127 '\n'
  (data (i32.const 2112) "\00")             ;; pointer to data: 0
  (data (i32.const 2116) "\01")             ;; length: 1
  (data (i32.const 2120) "\01")             ;; capacity: 1 bytes
  (data (i32.const 2124) "\01")             ;; stride: 1
  
  ;; 2128-2143 "> "
  (data (i32.const 2128) "\01")             ;; pointer to data: 1
  (data (i32.const 2132) "\02")             ;; length: 2
  (data (i32.const 2136) "\02")             ;; capacity: 2 bytes
  (data (i32.const 2140) "\01")             ;; stride: 1

  ;; 2144-2159 temp str
  (data (i32.const 2144) "\00")             ;; pointer to data: 0
  (data (i32.const 2148) "\00")             ;; length: 0
  (data (i32.const 2152) "\00")             ;; capacity: 0
  (data (i32.const 2156) "\00")             ;; stride: 1

  ;; 2160-2163 dynamic mem
  (data (i32.const 2160) "\00\10")          ;; points to next free byte, arbitrarily set it to 4096

  ;; 2164-2171 pointer to first pointer to static string, and number of pointers
  (data (i32.const 2164) "\00")             ;; first pointer to pointer, initialize to zero
  (data (i32.const 2168) "\00")             ;; num pointers, initialize to zero
  
  ;; use these offsets when saving byte arrays
  (global $offset_length        i32 (i32.const 4))
  (global $offset_capacity      i32 (i32.const 8))
  (global $offset_stride        i32 (i32.const 12))
  
  ;; locations of some buffers
  (global $stdin_buffer         i32 (i32.const 1024))
  (global $i32_dump             i32 (i32.const 2048))
  (global $i32_stdout_buffer    i32 (i32.const 2052))
  (global $true_buffer          i32 (i32.const 2080))
  (global $false_buffer         i32 (i32.const 2096))
  (global $newline_buffer       i32 (i32.const 2112))
  (global $prompt_buffer        i32 (i32.const 2128))
  (global $temp_str             i32 (i32.const 2144))
  (global $dyn_mem_ptr          i32 (i32.const 2160))
  (global $static_str_ptrs      i32 (i32.const 2164))

  (func (export "_start")
    call $read_input
    call $string_from_input
    call $write_string
    call $write_newline
    call $i32_from_input
    call $write_i32
    call $write_newline
    call $bool_from_input
    call $write_bool
    call $write_newline
    )
  
  ;; -----------------------------------------------------------------
  ;; Conversions
  ;; -----------------------------------------------------------------
  (func $atob (param $buffer i32) (result i32)
    ;; convert an array of bytes to a boolean value, represented as i32
    ;; assumptions:
    ;; * Only 0 is regarded as false, all other numbers are true, even negative ones
    ;; * This function does not try to handle text based values, i.e. it does not convert
    ;;   "true" or "false" strings to anything. Since it uses atoi, all non-numerical
    ;;   data will lead to garbage values, which in this case probably always yield true,
    ;;   since 0 is the only false value as per above.
    local.get $buffer
    call $atoi
    i32.eqz
    i32.eqz)

  (func $atof (param $buffer i32) (result f32)
    ;; TODO
    f32.const 0)
 
  (func $atoi (param $buffer i32) (result i32)
    ;; convert array of bytes to an i32 value
    (local $pdata i32)
    (local $pend i32)
    (local $n i32)
    (local $i i32)
    (local $v i32)
    (local $sign i32)
    ;; assumptions:
    ;; * $buffer points to a location with 8 consecutive bytes s.t.
    ;;   the first 4 point to the data and the next 4 contain the number of bytes
    ;; * The data is actually numerical data. Byte strings that are not of the form
    ;;   [0x2D] (0x30-0x39)* will result in gargabe values
    ;; * traps if length is 0 or if length is 1 and contains only '-' character or 0x2D byte
    
    ;; setup data pointer and len
    local.get $buffer
    i32.load
    local.set $pdata
    local.get $buffer
    call $array_len
    local.set $n
    local.get $buffer
    call $array_end
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

  (func $ftoa (param $v f32) (param $buffer i32)
    ;; TODO
    )
  
  (func $itoa (param $v i32) (param $buffer i32)
    ;; convert integer to a byte representation, i.e. an array of bytes
    (local $is_negative i32)
    (local $r i32)
    (local $i i32)
    (local $b i32)
    (local $pdata i32)
    ;; assumptions:
    ;; * $buffer is a pointer to memory s.t.
    ;;   first 4 bytes contain a pointer to data,
    ;;   next 4 bytes hold the number of bytes in data
    ;;   and last 4 bytes contain maximum capacity of data
    
    local.get $buffer
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
    local.get $buffer
    global.get $offset_length
    i32.add
    local.get $i
    local.get $pdata
    i32.sub
    i32.store)
 
  ;; -----------------------------------------------------------------
  ;; I/O
  ;; -----------------------------------------------------------------
  (func $write (param $buffer i32)
    ;; write an array of bytes starting at given loc to stdout
    ;; $buffer should point to a location of two consecutive i32 values,
    ;; the first of which points to the location of the data
    ;; and the second is the length of the data in bytes
    i32.const 1
    local.get $buffer
    i32.const 1
    global.get $i32_dump
    call $fd_write
    drop)
  
  (func $write_newline (export "write_newline")
    global.get $newline_buffer
    call $write)

  (func $writeln (param $buffer i32)
    local.get $buffer
    call $write 
    call $write_newline)

  (func $write_buffer_data (param $buffer i32)
    local.get $buffer
    i32.load
    call $write_i32
    call $write_newline
    local.get $buffer
    call $array_len
    call $write_i32
    call $write_newline
    local.get $buffer
    call $array_capacity
    call $write_i32
    call $write_newline
    local.get $buffer
    call $array_stride
    call $write_i32
    call $write_newline
    local.get $buffer
    call $writeln)
   
  (func $write_bool (export "write_bool") (param $value i32)
    (block
      (block
        local.get $value
        i32.eqz
        br_if 0
        global.get $true_buffer
        call $write
        br 1)
      global.get $false_buffer
      call $write))
  
  (func $write_i32 (export "write_i32") (param $v i32)
    local.get $v
    global.get $i32_stdout_buffer
    call $itoa
    global.get $i32_stdout_buffer
    call $reverse_bytes
    global.get $i32_stdout_buffer
    call $write)
  
  (func $write_f32 (export "write_f32") (param $v f32) 
    ;; TODO
    )
  
  (func $write_string (export "write_string") (param $buffer i32) 
    local.get $buffer
    call $write)

  (func $read_input (export "read_input")
    ;; reads the contents of stdin to the stdin array
    ;; the length of the buffer should be the maximum capacity always at start,
    ;; since that determines how many bytes are read from stdin
    ;; it is overwritten by the fd_read with the actual number of bytes read
    ;; '\n' is appended at the end, so reduce length by 1
    global.get $prompt_buffer
    call $write
    global.get $stdin_buffer
    global.get $offset_length
    i32.add
    global.get $stdin_buffer
    call $array_capacity
    i32.store

    ;; which buffer (in = 0, out = 1, err = 2)
    i32.const 0
    ;; where to read
    global.get $stdin_buffer
    ;; how many strings (keep as 1 always)
    i32.const 1
    ;; where to store the num of bytes read
    global.get $stdin_buffer
    global.get $offset_length
    i32.add
    ;; read
    call $fd_read
    drop
    ;; reduce length by 1 (drop the trailing '\n')
    global.get $stdin_buffer
    global.get $offset_length
    i32.add
    global.get $stdin_buffer
    call $array_len
    i32.const 1
    i32.sub
    i32.store)
  
  (func $bool_from_input (export "bool_from_input") (result i32)
    ;; converts the next whitespaced delimited value from stdin buffer to a bool(i32, [0, 1]) value (atob)
    ;; returns the converted value
    (local $v i32)
    call $set_temp_str_to_first_token_from_input
    global.get $temp_str
    call $atob
    local.set $v
    call $remove_temp_str_from_input
    local.get $v)
  
  (func $i32_from_input (export "i32_from_input")  (result i32)
    ;; converts the next whitespaced delimited value from stdin buffer to a i32 value (atoi)
    ;; returns the converted value
    (local $v i32)
    call $set_temp_str_to_first_token_from_input
    global.get $temp_str
    call $atoi
    local.set $v
    call $remove_temp_str_from_input
    local.get $v)
  
  (func $f32_from_input (export "f32_from_input") (result f32)
    ;; converts the next whitespaced delimited value from stdin buffer to a f32 value (atof)
    ;; returns the converted value
    (local $v f32)
    call $set_temp_str_to_first_token_from_input
    global.get $temp_str
    call $atof
    local.set $v
    call $remove_temp_str_from_input
    local.get $v)
  
  (func $string_from_input (export "string_from_input") (result i32)
    ;; creates a new string
    ;; adds the next whitespace delimited value from stdin buffer to that string
    ;; returns the address of the string
    (local $buffer i32)
    call $set_temp_str_to_first_token_from_input 
    i32.const 1 ;; stride 1 for string
    call $new_array
    local.tee $buffer
    global.get $temp_str
    call $copy_array
    drop
    call $remove_temp_str_from_input
    local.get $buffer)

  (func $remove_temp_str_from_input
    ;; use only with xx_from_input functions above
    global.get $stdin_buffer
    global.get $offset_length
    i32.add
    ;; src is first byte not used by us
    global.get $temp_str
    call $array_end
    ;; dst is start of stdin_buffer
    global.get $stdin_buffer
    i32.load
    ;; length is number of bytes in between
    global.get $stdin_buffer
    call $array_end
    global.get $temp_str
    call $array_end
    i32.sub
    call $copy
    i32.store)

  (func $set_temp_str_to_first_token_from_input
    (local $temp i32)
    ;; set temp_str length to stdin length
    global.get $temp_str
    global.get $offset_length
    i32.add
    global.get $stdin_buffer
    call $array_len
    i32.store
    ;; set tmp_str to start from first nonwhitespace from stdin
    global.get $temp_str
    global.get $stdin_buffer
    call $find_nonwhitespace
    local.tee $temp
    global.get $stdin_buffer
    i32.load
    i32.add
    i32.store
    ;; reduce the length of temp_str by the index of the first nonwhitespace
    global.get $temp_str
    global.get $offset_length
    i32.add
    global.get $temp_str
    call $array_len
    local.get $temp
    i32.sub
    i32.store
    ;; find first whitespace from the rest of the stdin string and reset temp_str length
    global.get $temp_str
    global.get $offset_length
    i32.add
    global.get $temp_str
    call $find_whitespace
    i32.store
    ;; set temp_str capacity to temp_str length
    global.get $temp_str
    global.get $offset_capacity
    i32.add
    global.get $temp_str
    call $array_len
    i32.store
    ;; set temp_str stride to 1
    global.get $temp_str
    global.get $offset_stride
    i32.add
    i32.const 1
    i32.store)
  
  ;; -----------------------------------------------------------------
  ;; String
  ;; -----------------------------------------------------------------
  (func $get_string_literal (export "get_string_literal") (param $idx i32) (result i32)
    ;; calculates the address of a saved static string from the given idx
    i32.const 16 ;; static strings save 16 bytes (4 x i32) of data
    local.get $idx
    i32.mul
    global.get $static_str_ptrs 
    i32.add)

  (func $string_eq (export "string_eq") (param $buffer1 i32) (param $buffer2 i32) (result i32)
    local.get $buffer1
    call $array_len
    local.get $buffer2
    call $array_len
    i32.eq)
  
  (func $string_neq (export "string_neq") (param $buffer1 i32) (param $buffer2 i32) (result i32)
    local.get $buffer1
    call $array_len
    local.get $buffer2
    call $array_len
    i32.ne)
 
  (func $string_great (export "string_great") (param $buffer1 i32) (param $buffer2 i32) (result i32)
    local.get $buffer1
    call $array_len
    local.get $buffer2
    call $array_len
    i32.gt_u)
  
  (func $string_great_eq (export "string_great_eq") (param $buffer1 i32) (param $buffer2 i32) (result i32)
    local.get $buffer1
    call $array_len
    local.get $buffer2
    call $array_len
    i32.ge_u)
  
  (func $string_less (export "string_less") (param $buffer1 i32) (param $buffer2 i32) (result i32)
    local.get $buffer1
    call $array_len
    local.get $buffer2
    call $array_len
    i32.lt_u)
  
  (func $string_less_eq (export "string_less_eq") (param $buffer1 i32) (param $buffer2 i32) (result i32)
    local.get $buffer1
    call $array_len
    local.get $buffer2
    call $array_len
    i32.le_u)

  (func $string_concatenate (export "string_concatenate") (param $buffer1 i32) (param $buffer2 i32) (result i32)
    ;; Takes two strings and adds the contents of the second
    ;; to the first, or as many bytes as fit within capacity.
    ;; This function does not allocate.
    ;; Returns $buffer1
 
    ;; get address of length on stack for the final store op
    local.get $buffer1
    global.get $offset_length
    i32.add   

    ;; src = $buffer2
    local.get $buffer2
    i32.load

    ;; dst = $buffer1 + str1.len (in bytes)
    local.get $buffer1
    call $array_end

    ;; str1.capacity - str1.len
    local.get $buffer1
    call $array_capacity
    local.get $buffer1
    call $array_len
    i32.sub

    ;; str2.len
    local.get $buffer2
    call $array_len

    ;; min(str2.len, str1.capacity - str1.len)
    call $i32_min

    ;; copy n bytes from $buffer2 to $buffer1 + len
    ;; returns num bytes copied
    call $copy
    local.get $buffer1
    call $array_len
    i32.add
    i32.store

    local.get $buffer1)

  ;; -----------------------------------------------------------------
  ;; Array
  ;; -----------------------------------------------------------------
  (func $array_data (param $buffer i32) (param $idx i32) (result i32)
    ;; returns the member of the array at buffer
    local.get $buffer
    local.get $idx
    i32.add
    i32.load)
  
  (func $array_len (param $buffer i32) (result i32)
    ;; returns the length of the array at buffer
    ;; get len
    local.get $buffer
    global.get $offset_length
    call $array_data)
  
  (func $array_capacity (param $buffer i32) (result i32)
    ;; returns the capacity of the array at buffer
    local.get $buffer
    global.get $offset_capacity
    call $array_data)

  (func $array_stride (param $buffer i32) (result i32)
    local.get $buffer
    global.get $offset_stride
    call $array_data)
  
  (func $array_size (export "array_size") (param $buffer i32) (result i32)
    ;; returns the length of the array at buffer divided by stride
    local.get $buffer
    call $array_len
    local.get $buffer
    call $array_stride
    i32.div_s)

  (func $array_end (param $buffer i32) (result i32)
    ;; returns the bufferess of the first free byte
    local.get $buffer
    i32.load
    local.get $buffer
    call $array_len
    i32.add)

  (func $array_access (param $buffer i32) (param $idx i32) (param $str_idx i32)  (result i32)
    ;; this function validates the array access (checks oob)
    ;; and returns the bufferess if everything is ok
    local.get $buffer
    local.get $idx
    local.get $str_idx
    call $check_bounds
    local.get $buffer
    call $array_stride
    i32.mul
    local.get $buffer
    i32.load
    i32.add)
  
  (func $array_access_i (export "array_access_i") (param $buffer i32) (param $idx i32) (param $str_idx i32)  (result i32)
    local.get $buffer
    local.get $idx
    local.get $str_idx
    call $array_access
    i32.load)
  
  (func $array_assign_i (export "array_assign_i") (param $buffer i32) (param $idx i32) (param $str_idx i32) (param $value i32)
    local.get $buffer
    local.get $idx
    local.get $str_idx
    call $array_access
    local.get $value
    i32.store)
  
  (func $array_access_f (export "array_access_f") (param $buffer i32) (param $idx i32) (param $str_idx i32)  (result f32)
    local.get $buffer
    local.get $idx
    local.get $str_idx
    call $array_access
    f32.load)
  
  (func $array_assign_f (export "array_assign_f") (param $buffer i32) (param $idx i32) (param $str_idx i32) (param $value f32)
    local.get $buffer
    local.get $idx
    local.get $str_idx
    call $array_access
    local.get $value
    f32.store)

  (func $allocate (export "allocate") (param $n_bytes i32) (result i32)
    ;; allocates n_bytes from linear memory
    ;; new allocation always starts from a 16 byte aligned address
    ;; returns the address
    (local $start i32)
    (local $temp i32)
    global.get $dyn_mem_ptr
    global.get $dyn_mem_ptr
    i32.load
    local.tee $temp
    i32.const 16
    i32.rem_u
    local.get $temp
    i32.add
    local.tee $start
    local.get $n_bytes
    i32.add
    i32.store
    local.get $start)

  (func $check_bounds (export "check_bounds") (param $buffer i32) (param $idx i32) (param $str_idx i32) (result i32)
    ;; checks that the length of array at $buffer >= $idx
    ;; returns $idx if yes, otherwise prints the literal string at $str_idx and becomes unreachable

    ;; assumptions:
    ;; - buffer points to a location that contains 4 32 bit values:
    ;;   - 0 = pointer to data
    ;;   - 1 = length
    ;;   - 2 = capacity
    ;;   - 3 = stride, i.e. how many bytes one value takes
    ;; - idx is in units of stride, not of bytes,
    ;;   i.e. i = 1 -> bytes 4-7
    (block
      (block
        local.get $buffer
        call $array_len
        local.get $idx
        local.get $buffer
        call $array_stride
        i32.mul
        i32.gt_u
        br_if 1)
      local.get $str_idx
      call $get_string_literal
      call $write_string
      unreachable)
    local.get $idx)

  (func $new_array (export "new_array") (param $stride i32) (result i32)
    ;; allocates 1024 consecutive bytes
    ;; first 16 bytes hold four i32 values: pdata, length, capacity, stride
    ;; sets pdata to point to the addr + 16 address, set length to 0,
    ;; stride to stride and capacity to 1008
    ;; returns a pointer to the first of the four consecutive i32 values
    (local $buffer i32)
    ;; allocate
    i32.const 1024
    call $allocate
    ;; set pdata
    local.tee $buffer
    local.get $buffer
    i32.const 16
    i32.add
    i32.store
    ;; set length to 0
    local.get $buffer
    global.get $offset_length
    i32.add
    i32.const 0
    i32.store
    ;; set capacity to 1008 (= 1024 - 16)
    local.get $buffer
    global.get $offset_capacity
    i32.add
    i32.const 1008
    i32.store
    ;; set stride to stride
    local.get $buffer
    global.get $offset_stride
    i32.add
    local.get $stride
    i32.store

    local.get $buffer)

  (func $copy_array (export "copy_array") (param $dst_buffer i32) (param $src_buffer i32) (result i32)
    ;; copies values of length and capacity from src to dst
    ;; copies bytes from src pdata to dst pdata
    ;; returns the value dst
    local.get $dst_buffer
    global.get $offset_length
    i32.add
    local.get $src_buffer
    call $array_len
    i32.store

    local.get $dst_buffer
    global.get $offset_capacity
    i32.add
    local.get $src_buffer
    call $array_capacity
    i32.store

    local.get $dst_buffer
    global.get $offset_stride
    i32.add
    local.get $src_buffer
    call $array_stride
    i32.store

    local.get $src_buffer
    i32.load
    local.get $dst_buffer
    i32.load
    local.get $src_buffer
    call $array_len
    call $copy
    drop
    local.get $dst_buffer)
 
  ;; -----------------------------------------------------------------
  ;; Misc
  ;; -----------------------------------------------------------------
  (func $reverse_bytes (param $buffer i32)
    ;; reverse bytes in place.
    (local $i i32)
    (local $j i32)
    (local $n i32)
    (local $n2 i32)
    (local $t i32)
    (local $pdata i32)
    ;; assumptions:
    ;; * $buffer contains 8 bytes of data, with first 4 bytes pointing
    ;;   to location of data and next 4 bytes being the number of bytes at that loc
    ;; * works for strings, if string consists of one byte characters
    
    local.get $buffer
    i32.load
    local.set $pdata
    local.get $buffer
    call $array_len
    local.set $n
    
    (block
      ;; byte strings of lenght 0 and 1 are their own reverses, early exit
      local.get $n
      call $is_one_or_zero
      br_if 0
    
      ;; i points to first byte 
      local.get $pdata
      local.set $i
    
      ;; n2 points to midpoint, i.e. floor(n/2) + buffer
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
  
  (func $i32_min (param i32) (param i32) (result i32)
    (local $min i32)
    local.get 0
    local.set $min
    (block
      local.get 0
      local.get 1
      i32.lt_u
      br_if 0
      local.get 1
      local.set $min)
    local.get $min)
  
  (func $i32_max (param i32) (param i32) (result i32)
    (local $max i32)
    local.get 0
    local.set $max
    (block
      local.get 0
      local.get 1
      i32.gt_u
      br_if 0
      local.get 1
      local.set $max)
    local.get $max)
  
  (func $is_one_or_zero (param i32) (result i32)
    ;; check if given integer is zero everywhere except LSB
    local.get 0
    i32.const 0xfffffffe
    i32.and
    i32.eqz)

  (func $is_whitespace (param $addr i32) (result i32)
    (local $v i32)
    (local $temp i32)
    local.get $addr
    i32.load8_u
    
    ;; space
    local.tee $v
    i32.const 0x20
    i32.eq
    local.get $temp
    i32.or
    local.set $temp
    
    ;; tab
    local.get $v
    i32.const 0x09
    i32.eq
    local.get $temp
    i32.or
    local.set $temp
    
    ;; line feed
    local.get $v
    i32.const 0x0A
    i32.eq
    local.get $temp
    i32.or)

  (func $find_nonwhitespace (param $buffer i32) (result i32)
    ;; finds the index of the first nonwhitespace from buffer
    ;; if there are only whitespaces, returns the length of buffer
    (local $i i32)
    (local $n i32)
    (local $addr i32)
    i32.const 0
    local.set $i
    local.get $buffer
    call $array_len
    local.set $n
    local.get $buffer
    i32.load
    local.set $addr
    (block
      (loop
        local.get $addr
        local.get $i
        i32.add
        call $is_whitespace
        i32.eqz
        local.get $i
        local.get $n
        i32.ge_u
        i32.or
        br_if 1
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0))
    local.get $i)

  (func $find_whitespace (param $buffer i32) (result i32)
    ;; finds the index of the first whitespace from buffer
    ;; if there are no whitespaces, returns the length of buffer
    (local $i i32)
    (local $n i32)
    (local $addr i32)
    i32.const 0
    local.set $i
    local.get $buffer
    call $array_len
    local.set $n
    local.get $buffer
    i32.load
    local.set $addr
    (block
      (loop
        local.get $addr
        local.get $i
        i32.add
        call $is_whitespace
        local.get $i
        local.get $n
        i32.ge_u
        i32.or
        br_if 1
        local.get $i
        i32.const 1
        i32.add
        local.set $i
        br 0))
    local.get $i)

  (func $copy (param $src i32) (param $dst i32) (param $n_bytes i32) (result i32)
    ;; returns the number of bytes copied
    (local $i i32)
    i32.const 0
    local.set $i
    (block
      (loop
        local.get $dst
        local.get $i
        i32.add
        local.get $src
        local.get $i
        i32.add
        i32.load8_s
        i32.store8
        local.get $i
        i32.const 1
        i32.add
        local.tee $i
        local.get $n_bytes
        i32.ge_u
        br_if 1
        br 0))
    local.get $i)
)
