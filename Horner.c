#include <stdio.h>

//x^3 + 3x^2 + 2x + 5
//x (2 + 3x + x^2) + 5
//x (2 + x(3 + x)) + 5
//((x + 3)x + 2)x + 5
int main()
{
    int n;
    printf ("Grau do polinomio: ");
    scanf ("%d", &n);
    
    printf ("Digite os coeficientes em ordem descrescente de grau: ");
    int coef[n];
    for (int i = 0; i <= n; i++)
    {
        scanf ("%d", &coef[i]);
    }
    int x;
    printf ("Digite o valor de x: \n");
    scanf ("%d", &x);
    
    int p = coef[n];
    
    for (int i = n; i > 0; i--)
    {
        //printf ("Antes: p = %d, coef = %d\n", p, coef[i-1]);
        p = (p*x) + coef[i-1];
        //printf ("Depois: p = %d\n", p);
    }
    printf ("resultado = %d", p);
    return 0;
}
