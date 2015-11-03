# Each "cell" is (char isValid isUsed)

# First cell is (0 0 0)
>>>

# Read each character as cells of the form (char 1 0)
,[>+>>,]

# Last input cell is of the form (0 0 0)
>>>

# First output cell starts in the form (0 1 0)
>+<

# Go back to the last input cell
<<<<<<

# Loop while the current isValid field is nonzero
>[
  # Loop while the current char is nonzero
  <[
    # Decrement the char
    -
    # Go to the first output cell
    >[>>>]>>
    # Go to the last output cell
    >[>>>]<<<<
    # Increment the output character
    +
    # Go back to the last input cell
    >[<<<]<<<<
    # Go to the last unused input cell
    >>[<<<]<<
  ]

  # Go to the first output cell
  >[>>>]>>
  # Go to the last output cell
  >[>>>]<<<<
  # Add an isValid field to the next output cell
  >>>>+<<<<
  # Go back to the last input cell
  >[<<<]<<<<
  # Go to the last unused input cell
  >>[<<<]<<

  # Set the isUsed field to 1
  >>+
  # Go to the isValid field of the previous input cell
  <<<<
]

# We are now at the first cell in memory
# We would like to print out our solution

# Go up to the first output cell
>>>[>>>]>>

# Print out every char cell
[.>>>]

