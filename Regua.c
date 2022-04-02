#include <stdio.h>
#include <math.h>

void function (int R[], int inicio, int fim, int quant)
{
    int meio = 0;
    if (inicio == fim-1)
    {
        return;
    }
    else
    {
        meio = (inicio + fim)/2;
        R[meio] = quant;
        function (R, inicio, meio, quant-1);
        function (R, meio, fim, quant-1);
    }
    
}

//0 1 2 1 3 1 2 1 4 1 2 1 3 1 2 1 0

int main()
{
    int n;
    scanf ("%d", &n);
    double x = pow(2, n);
    int tam = x+1;
    int R[tam];
    
    for(int i = 0; i < tam; i++)
    {
        R[i] = 0;
    }
    for(int i = 0; i < tam; i++)
    {
        printf ("%d ", R[i]);
    }
    printf ("\n");
    
    function (R, 0, tam-1, n);
    
    for(int i = 0; i < tam; i++)
    {
        printf (". ");
        for (int j = 0; j < R[i]; j++)
        {
            printf ("- ");
        }
        printf ("\n");
    }
    return 0;
}
