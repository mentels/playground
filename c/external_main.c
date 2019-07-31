#include <stdio.h>
#include "external.h"

int main(void) {
  char* name = my_name();
  printf("Hello %s", name);
}

boolean is_pin_correct = false;
int max_retries = 3;
int p;
while (not is_pin_correct) && max_retries > 0 {
  p = pin();
  if pin_correct(p) {
      is_pin_correct = true
    }
  else {
    max_retires--
  }
}

if(is_pin_correct == false) {
  printf
  exit(1);
 }

switch(menu()) {

 }
