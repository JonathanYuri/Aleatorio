#include <stdio.h>

int diferente(int vetor[], int i, int cont)
{
    if (cont >= 9)  return 1;
    if (i == cont)  cont++;
    if (vetor[i] == vetor[cont])    return 0;
    else    return diferente(vetor, i, cont+=1);
}

int todos_positivos(int vetor[], int cont)
{
    if (cont >= 9)  return 1;
    if (vetor[cont] <= 0)   return 0;
    else    return todos_positivos(vetor, cont+1);
}

void function(int i, int h)
{
    int a, b, c, d, e = 5, f, g;
    a = 10 - i;
    b = 10 - h;
    c = h + i - 5;
    d = h + 2*i - 10;
    f = 20 - h - 2*i;
    g = 15 - h - i;
    
    int vetor[9] = {a, b, c, d, e, f, g, h, i};
    int dif = -1, error = 0;
    
    // todos diferente
    for (int i = 0; i < 9; i++)
    {
        dif = diferente(vetor, i, 0);
        
        if (dif == 0)
        {
            error = 1;
            break;
        }
    }
    
    // todos positivos
    int TP = todos_positivos(vetor, 0);
    
    if (error == 0 && TP == 1)
    {
        printf("%i %i %i\n%i %i %i\n%i %i %i\n\n", a, b, c, d, e, f, g, h, i);
    }
}

int main()
{
    // a + i = 10
    // b + h = 10
    // c - h - i = -5
    // d - h - 2i = -10
    // e = 5
    // f + h + 2i = 20
    // g + h + i = 15
    
    for (int i = 1; i < 10; i++)
    {
        for (int h = 1; h < 10; h++)
        {
            function(i, h);
        }
    }

    return 0;
}
