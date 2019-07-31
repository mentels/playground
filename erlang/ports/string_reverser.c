#include <stdio.h>
#include <string.h>
#define BUFFER_LENGTH 80

void reverse_string(char* str);
void drop_newline_char(char* str);

int main() {
  char line[BUFFER_LENGTH];
  while (1) {
    // at most BUFFER_LENGTH characters are read (i.e. 79)
    if (fgets(line, BUFFER_LENGTH, stdin) != NULL) {
      drop_newline_char(line);
      reverse_string(line);
      printf("%s\n", line);
      fflush(stdout);
    } else {
      return 0;
    }
  }
}

void reverse_string(char* str) {
  size_t len = strlen(str);
  size_t i,k;
  char temp;
  for(i = 0, k=len-1; i < (len/2); i++, k--)
    {
      temp = str[k];
      str[k] = str[i];
      str[i] = temp;
    }
}

void drop_newline_char(char* str) {
  size_t len = strlen(str);
  if (str[len-1] == '\n') {
    str[len-1] = '\0';
  }
}
