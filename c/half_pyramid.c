#include<stdio.h>

int main(int argc, char** argv) {
  int i, j, rows;

  printf("Enter the number of row: ");
  scanf("%d", &rows);

  for(i=1; i<=rows; ++i) {
    for(j=1; j<=i; ++j) {
      printf("*");
    }
    printf("\n");
  }
}
