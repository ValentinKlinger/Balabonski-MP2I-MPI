// Exercice 23 Écrire une fonction void swap(int *x, int *y) qui échange les
// valeurs de *x et *y. L’utiliser pour échanger les valeurs de deux variables locales
// de type int de la fonction main et afficher leurs valeurs pour vérifier que l’échange
// est bien réalisé. Écrire de même une fonction void minmax(int *x, int *y) qui
// met dans *x la plus petite des deux valeurs *x et *y et dans *y la plus grande.

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

void swap(int *x, int *y)
{
    int new_x = *y;
    *y = *x;
    *x = new_x;
}

void minmax(int *x, int *y)
{
    if (*x > *y)
    {
        swap(x, y);
    }
}

int main()
{
    int x = 21;
    int y = 2;
    printf("x :%d, y :%d\n", x, y);
    swap(&x, &y);
    printf("x :%d, y :%d\n", x, y);
    minmax(&x, &y);
    printf("x :%d, y :%d\n", x, y);
}

// Correction du manuel
// void swap(int *x, int *y) {
//     int tmp = *x;
//     *x = *y;
//     *y = tmp;
// }
//
// void minmax(int *x, int *y) {
//     if (*x > *y)
//         swap(x, y);
// }
//
// int main() {
//     int a = 55, b = 89;
//     swap(&a, &b);
//     printf("*a=%d, *b=%d\n", a, b);
// }