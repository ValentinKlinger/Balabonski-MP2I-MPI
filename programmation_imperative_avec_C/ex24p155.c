// Exercice 24 Écrire une fonction bool is_sorted(int a[], int n) qui renvoie
// true si et seulement si le tableau a de taille n est trié en ordre croissant.

#include <assert.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

bool is_sorted(int a[], int n)
{
    while (n > 1)
    {
        if (a[n - 2] > a[n - 1])
        {
            return false;
        }
        n -= 1;
    }
    return true;
}

int main()
{
    // int tableau[5] = {1, 3, 5, 7, 19};
    // int taille = 5;
    // printf("%d", is_sorted(tableau, taille));
}

// Correction proposé dans le manuel :
// bool is_sorted(int a[], int n) {
//   for (int i = 0; i < n-1; i++)
//     if (a[i] > a[i+1])
//       return false;
//   return true;
// }