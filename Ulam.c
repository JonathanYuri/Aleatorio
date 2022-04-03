#include <stdio.h>

int main()
{
    int x, cont = 1, M_seq = 0, num_maior = 0, media = 0, interacao = 0;
    
    for (int i = 1; i <= 65535; i++)
    {
        interacao += 1;
        x = i;
        while (x != 1)
        {
            if (x % 2 == 0)
            {
                //printf ("%d ", x);
                x = x/2;
                cont++;
            }
            else
            {
                //printf ("%d ", x);
                x = (3 * x) + 1;
                cont++;
            }
        }
        //printf ("1\n");
        media += cont;
        if (M_seq < cont)
        {
            num_maior = i;
            M_seq = cont;
        }
        cont = 1;
    }
    //printf ("Media = %d Interacao = %d\n", media, interacao);
    media = media/interacao;
    printf ("Maior Sequencia = %d\nNumero Maior = %d\nMedia = %d", M_seq, num_maior, media);
    
    return 0;
}
