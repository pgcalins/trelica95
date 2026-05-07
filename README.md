**trelica95: Programa educacional para análise estática de treliças planas** (Educational program for static analysis of plane trusses)

O programa trelica95.f95 é uma adaptação (fork) do programa de treliças planas de BREBBIA & FERRANTE (1986). O programa original estava escrito em Fortran IV/77 e foi adaptado para Fortran 90/95, com uma disciplina de programação estruturada mais forte.

O programa possui a possibilidade de ser uma boa iniciação para a programação de métodos matriciais em estruturas ou para elementos finitos lineares.

Os vetores são alocados de forma dinâmica na execução.

A conectividade é alocada no vetor kon. A estrutura de dados é a mesma usada por BREBBIA & FERRANTE (1986).

O programa possui quatro módulos: (1) variáveis; (2) Entrada_Saida; (3) Matriz_Elemento; e (4) Matriz_Global.

O módulo variáveis permite guardar as variáveis do programa como globais.

O módulo Entrada_Saida possui as sub-rotinas de Entrada_Dados(); Imprime_Resultados() e Resultados_GiD(), esta última escreve os dados no arquivo de pós processamento do GiD (CIMNE, 2026).

O módulo Matriz_Elemento possui as subrotinas Monta_Matriz_Elemento() e Calcula_Incognitas_Secundarias(). Na subrotina Monta_Matriz_Elemento() está implementada a formulação da matriz de rigidez do elemento de treliça. 

O módulo Matriz_Global possui as subrotinas Monta_Matriz_Global(); Armazena_Elemento_na_Global(); Impoe_Condicoes_de_Contorno() e Sistema_Linear_Gauss(). A subrotina Sistema_Linear_Gauss() resolve o sistema linear pelo método de Gauss, possuindo como critério de zero na diagonal principal uma valor menor que 1.0E-7.

Para que o pré processamento no GiD seja possível o diretório trelica95.gid/ deve ser copiado no diretório problemtypes/ do GiD.


**Referências Bibliográficas**

BREBBIA, C.A. & FERRANTE, A.J. (1986) **Computational methods for the solution of engineering problems**. London: Pentech Press. 370p.
 
CIMNE (2026) **GiD simulation**. Disponível em: https://www.gidsimulation.com/. Acesso em: 07 maio 2026.
