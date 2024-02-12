# Dev Notes

## To-Do

- V2 currently doesn't work
  - Source does not have data for weekend dates
    - To handle this, recode the script so that if sysdate is a weekend date, then the code should use the most recent weekday instead
- Current loop structure (which should be replaced) causes an error

## Other Notes

- This code will need to be edited when integrated into main file, since currently it prints the value out (final version will only need to save the variable so that it can be used in the main script)
