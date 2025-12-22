!
!     trelica95  Analise Estatica de Trelicas Planas
!     =========
!
!     Adaptado de:
!     BREBBIA,C.A. & FERRANTE,A.J. (1986) Computational methods for the
!     solution of engineering problems. London: Pentech Press. 370p.
!
!     P¢s-processamento no GiD https://www.gidhome.com/
!
!     Implementado por:
!     Paulo Gustavo Cavalcante Lins <pgcalins@gmail.com>
!
module variaveis
implicit none

  integer, parameter :: ndf=2 !! numero de graus de liberdade por n¢
  integer, parameter :: nne=2 !! numero de n¢s por elemento
  integer, parameter :: ndfel=ndf*nne !! graus de liberdade por elemento

  integer, parameter :: in=15 !! numero do arquivo de entrada
  integer, parameter :: io=16 !! numero do arquivo de saida

  integer :: nnode !! numero de n¢s
  real(8), allocatable :: X(:),Y(:) !! coordenadas dos n¢s
  integer :: nelem !! numero de elementos
  integer, allocatable :: kon(:) !! conectividade dos elementos
  integer, allocatable :: imat(:) !! numero do material do elemento
  integer :: nmat !! numero de materiais
  real(8), allocatable :: Ei(:) !! modulo de elasticidade do elemento
  real(8), allocatable :: Area(:) !! area do elemento
  integer :: nln !! numero de nos carregados
  integer :: nbn !! numero de nos com condicao de contorno
  integer, allocatable :: istatus(:) !! indicador de status
  real(8), allocatable :: Prescrito(:) !! deslocamentos prescritos

  integer :: neq !! numero total de incognitas
  real(8), allocatable :: Carga(:) !! Vetor de cargas nodais
  real(8), allocatable :: Desloc(:) !! Vetor de deslocamentos nodais
  real(8), allocatable :: RigGlobal(:,:) !! Matriz de rigidez global
  real(8) :: RigElem(ndfel,ndfel) !! Matriz de rigidez do elemento

  real(8), allocatable :: Reac(:) !! Vetor de reacoes nodais
  real(8), allocatable :: Forc(:) !! Vetor de forca axial no elemento

end module variaveis



module Entrada_Saida

contains

subroutine Abre_Arquivos()
use variaveis
   open(in,file='NomeArq.dat',status='old')
   open(io,file='NomeArq.out',status='unknown')
   open(33,file='NomeArq.post.msh',status='unknown')
   open(34,file='NomeArq.post.res',status='unknown')
   return
end subroutine Abre_Arquivos



subroutine Entrada_Dados()
use variaveis
implicit none
integer :: i,j,ic(nne),n1,k,k1,L1,L2,n2
real(8) :: w(ndf)

   write(io,'(A,/)')' DADOS DE ENTRADA'

   read(in,*)nnode
   write(io,'(A,i5)')' numeros de nos          :',nnode
   write(io,'(/,A)')' coordenadas nodais'
   write(io,'(7x,A,6x,A,9x,A)')' no ','x','y'
   allocate(X(nnode)); allocate(Y(nnode))
   do j=1,nnode
      read(in,*)i,X(i),Y(i)
      write(io,'(i10,2f10.2)')i,X(i),Y(i)
   enddo

   read(in,*)nelem
   write(io,'(/,A,i5/)')' numero de elementos     :',nelem
   write(io,*)' conectividade dos elementos e propriedades'
   write(io,'(A,3x,A,5x,A)')'elemento','no inicial  no final','material'
   allocate(kon(nne*nelem))
   allocate(imat(nelem))
   do j=1,nelem
      read(in,*)i,ic(1),ic(2),imat(i)
      write (io,'(4i10)') i,ic(1),ic(2),imat(i)
      n1=nne*(i-1)
      kon(n1+1)=ic(1)
      kon(n1+2)=ic(2)
   enddo

   read(in,*)nmat
   write(io,'(/,A,i5/)')' numero de materiais     :',nmat
   allocate(Ei(nmat))
   allocate(Area(nmat))
   do j=1,nmat
      read(in,*)i,Ei(i),Area(i)
      write(io,'(i10,2f20.5)')i,Ei(i),Area(i)
   enddo

   neq=nnode*ndf  !! calcula o numero total de incognitas
   allocate(Carga(neq))
   Carga(:)=0.0
   allocate(Desloc(neq))
   Desloc(:)=0.0
   do i=1,neq
      Carga(i)=0.0 !! Zera vetor de cargas nodais
   enddo

   read(in,*)nln
   write(io,'(/,A,i5/)')' numero de nos carregados:',nln
   write(io,'(A,/7x,A,7x,A,8x,A)')'cargas nodais','no','px','py'
   do i=1,nln
      read(in,*) j,(w(k),k=1,ndf)
      write(io,'(i10,2f10.2)') j,(w(k),k=1,ndf)
      do k=1,ndf
         k1=ndf*(j-1)+k
         Carga(k1)=w(k)
      enddo
   enddo

   read(in,*)nbn
   write(io,'(//,A,i5//)')' numero de nos suportados:',nbn
   write(io,*)' dados das condicoes de contorno'
   write(io,'(23X,A,14X,A)')'status','valores prescritos'
   write(io,'(16X,A)')'(1:prescrito, 0:livre)'
   write(io,'(7x,A,10x,A,9x,A,16x,A,9x,A)')'no','u','v','u','v'
   allocate(istatus((ndf+1)*nbn))
   allocate(Prescrito(neq))
   do i=1,nbn
      read(in,*) j,(ic(k),k=1,ndf),(w(k),k=1,ndf)
      write(io,'(3i10,10x,2f10.4)') j,(ic(k),k=1,ndf),(w(k),k=1,ndf)
      L1=(ndf+1)*(i-1)+1
      L2=ndf*(j-1)
      istatus(L1)=j
      do k=1,ndf
         n1=L1+k
         n2=L2+k
         istatus(n1)=ic(k)
         Prescrito(n2)=w(k)
      enddo
   enddo

return
end subroutine Entrada_Dados



subroutine Imprime_Resultados()
use variaveis
implicit none
integer :: i,j,k1,k2

   write(io,'(//,A,/)')' RESULTADOS'

   write(io,'(A,/)')' deslocamentos nodais'
   write(io,'(8X,A,12X,A,14x,A)')'no','u','v'
   do i=1,nnode
      k1=ndf*(i-1)+1
      k2=k1+ndf-1
      write(io,'(i10,2E20.6)') i,(Desloc(j),j=k1,k2)
   enddo

   write(io,'(/,A,/)')' reacoes nodais'
   write(io,'(8X,A,10X,A,13x,A)')'no','Px','Py'
   do i=1,nnode
      k1=ndf*(i-1)+1
      k2=k1+ndf-1
      write(io,'(i10,2f15.4)') i,(Reac(j),j=k1,k2)
   enddo

   write(io,'(/,A,/)')' forca nos membros'
   write(io,'(6X,A)')'elemento        forca axial'
   do i=1,nelem
      write(io,'(i10,f15.4)') i,Forc(i)
   enddo

   return
end subroutine Imprime_Resultados



subroutine Resultados_GiD()
use variaveis
implicit none
integer :: i,j,n1,k1,k2

   !! Escreve arquivo da malha
   write(33,*)'MESH "Malha" dimension 2 ElemType Linear Nnode 2'
   write(33,*)'Coordinates'
   do i=1,nnode
      write(33,*)i,'  ',x(i),'  ',y(i)
   enddo
   write(33,*)'end coordinates'
   write(33,*)'Elements'
   do i=1,nelem
      n1=nne*(i-1)
      write(33,'(4(2X,I8))') i,kon(n1+1),kon(n1+2),imat(i)
   enddo
   write(33,*)'end elements'

   !! Escreve arquivo de resultados
   write(34,*)'GiD Post Results File 1.0'
   write(34,*)'GaussPoints "Malha_gauss" ElemType Linear "Malha"'
   write(34,*)'Number Of Gauss Points: 1'
   write(34,*)'Nodes not included'
   write(34,*)'Natural Coordinates: Internal'
   write(34,*)'End gausspoints'
   write(34,*)'Result "Desloc" "Load Analysis" ',1,' Vector OnNodes'
   write(34,*)'ComponentNames "X-Desloc", "Y-Desloc"'
   write(34,*)'Values'
   do i=1,nnode
      k1=ndf*(i-1)+1
      k2=k1+ndf-1
      write(34,'(i10,2(2X,E15.5))')i,(Desloc(j),j=k1,k2)
   enddo
   write(34,*)'End Values'
   write(34,*)'Result "Reac" "Load Analysis" ',1,' Vector OnNodes'
   write(34,*)'ComponentNames "X-Reac", "Y-Reac"'
   write(34,*)'Values'
   do i=1,nnode
      k1=ndf*(i-1)+1
      k2=k1+ndf-1
      write(34,'(i10,2(2X,E15.5))')i,(Reac(j),j=k1,k2)
   enddo
   write(34,*)'End Values'
   write(34,*)'Result "Forc" "Load Analysis" ',1,&
              & ' Scalar OnGaussPoints "Malha_gauss"'
   write(34,*)'Values'
   do i=1,nelem
      write(34,'(i10,E15.4)') i,Forc(i)
   enddo
   write(34,*)'End Values'

   return
end subroutine Resultados_GiD



subroutine Fecha_Arquivos()
use variaveis
   close(in)
   close(io)
   close(33)
   close(34)
   return
end subroutine Fecha_Arquivos


end module Entrada_Saida



module Matriz_Elemento

contains

subroutine Monta_Matriz_Elemento(nel)
use variaveis
implicit none
integer :: nel
integer :: i,j,L,n1,n2,k1,k2
real(8) :: d,si,co,coef

   L=nne*(nel-1) !! nel = numero do elemento corrente
   n1=kon(L+1)   !! numero do n¢ inicial
   n2=kon(L+2)   !! numero do n¢ final

   d=sqrt((x(n2)-x(n1))**2+(y(n2)-y(n1))**2) !! comprimento do elemento
   co=(x(n2)-x(n1))/d !! cosseno do eixo do x local
   si=(y(n2)-y(n1))/d !! seno do eixo do x local
   coef=ei(imat(nel))*area(imat(nel))/d !! Coeficiente de rigidez
   !! calcula a matriz de rigidez do elemento
   RigElem(1,1)=coef*co*co
   RigElem(1,2)=coef*co*si
   RigElem(2,1)=coef*co*si
   RigElem(2,2)=coef*si*si
   do i=1,2
      do j=1,2
        k1=i+ndf
        k2=j+ndf
        RigElem(k1,k2)=RigElem(i,j)
        RigElem(i,k2)=-RigElem(i,j)
        RigElem(k1,j)=-RigElem(i,j)
      enddo
   enddo

   return
end subroutine Monta_Matriz_Elemento



subroutine Calcula_Incognitas_Secundarias()
use variaveis
implicit none
integer :: i,nel,L,n1,n2,k1,k2
real(8) :: d,si,co,coef

   allocate(Reac(neq))
   do i=1,neq
      Reac(i)=0.0
   enddo
   allocate(Forc(nelem))

   do nel=1,nelem  !! nel=numero do elemento corrente
      L=nne*(nel-1)
      n1=kon(L+1) !! numero do n¢ inicial para o elemento corrente
      n2=kon(L+2) !! numero do n¢ final para o elemento corrente
      k1=ndf*(n1-1)
      k2=ndf*(n2-1)
      d=sqrt((x(n2)-x(n1))**2+(y(n2)-y(n1))**2) !! comprimento do elemento
      co=(x(n2)-x(n1))/d !! cosseno do eixo do x local
      si=(y(n2)-y(n1))/d !! seno do eixo do x local
      coef=Ei(imat(nel))*Area(imat(nel))/d !! Coeficiente de rigidez
      !! Calcula a for‡a axial no membro e armazena no vetor forc
      Forc(nel)=coef*((Desloc(k2+1)-Desloc(k1+1))*co &
                & +(Desloc(k2+2)-Desloc(k1+2))*si)
      !! Calcula proje‡Æo global da for‡a axial do membro e adiciona
      !! ao vetor de rea‡äes Reac
      Reac(k1+1)=Reac(k1+1)-Forc(nel)*co
      Reac(k1+2)=Reac(k1+2)-Forc(nel)*si
      Reac(k2+1)=Reac(k2+1)+Forc(nel)*co
      Reac(k2+2)=Reac(k2+2)+Forc(nel)*si
   enddo

   return
end subroutine Calcula_Incognitas_Secundarias


end module Matriz_Elemento



module Matriz_Global

contains

subroutine Monta_Matriz_Global()
use variaveis
use Matriz_Elemento
implicit none
integer :: i,j,nel

    allocate(RigGlobal(neq,neq))
    do i=1,neq
       do j=1,neq
          RigGlobal(i,j)=0.0  !! Zera matriz global
       enddo
    enddo

    do nel=1,nelem
       call Monta_Matriz_Elemento(nel)
       call Armazena_Elemento_na_Global(nel)
    enddo

   return
end subroutine Monta_Matriz_Global




subroutine Armazena_Elemento_na_Global(nel)
use variaveis
implicit none
integer :: nel
integer :: i,j,i1,j1,i2,j2,n1,n2,k,L,jr,kr,jc,kc

   do i=1,nne
      n1=kon(nne*(nel-1)+i)
      i1=ndf*(i-1)
      j1=ndf*(n1-1)
      do j=1,nne
         n2=kon(nne*(nel-1)+j)
         i2=ndf*(j-1)
         j2=ndf*(n2-1)
         do k=1,ndf
            jr=i1+k
            kr=j1+k
            do L=1,ndf
               jc=i2+L
               kc=j2+L
               RigGlobal(kr,kc)=RigGlobal(kr,kc)+RigElem(jr,jc)
            enddo
         enddo
      enddo
   enddo

   return
end subroutine Armazena_Elemento_na_Global



subroutine Impoe_Condicoes_de_Contorno()
use variaveis
implicit none
integer :: L,no,k1,i,kr,j

   do L=1,nbn
      no=istatus((ndf+1)*(L-1)+1)
      k1=ndf*(no-1)
      do i=1,ndf
         if (istatus((ndf+1)*(L-1)+1+i).eq.1) then
            kr=k1+i
            do j=1,neq
               Carga(j)=Carga(j)-RigGlobal(kr,j)*Prescrito(kr)
               RigGlobal(kr,j)=0.0
               RigGlobal(j,kr)=0.0
            enddo
            RigGlobal(kr,kr)=1.0
            Carga(kr)=Prescrito(kr)
         endif
      enddo
   enddo

   return
end subroutine Impoe_Condicoes_de_Contorno



subroutine Sistema_Linear_Gauss(n,A,B,X,io)
implicit none
integer :: n,io
real(8) :: A(n,n),B(n),X(n)
integer :: i,j,k
real(8) :: c

   do k=1,(n-1)
      c=A(k,k)
      if (abs(c).lt.1.0E-7) then
         write(io,*)'Singularidade na linha ',k
         return
      endif
      do j=1,n
         A(k,j)=A(k,j)/c
      enddo
      B(k)=B(k)/c
      do i=(k+1),n
         c=A(i,k)
         do j=1,n
            A(i,j)=A(i,j)-c*A(k,j)
         enddo
         B(i)=B(i)-c*B(k)
      enddo
   enddo
   if (abs(A(n,n)).lt.1.0E-7) then
      write(io,*)'Singularidade na linha ',k
      return
   endif
   B(n)=B(n)/A(n,n)
   do i=(n-1),1,-1
      do j=(i+1),n
         B(i)=B(i)-A(i,j)*B(j)
      enddo
   enddo
   do i=1,n
      X(i)=B(i)
   enddo

   return
end subroutine Sistema_Linear_Gauss


end module Matriz_Global



program trelica95
use variaveis
use Entrada_Saida
use Matriz_Elemento
use Matriz_Global
implicit none

   call Abre_Arquivos()
   call Entrada_Dados()
   call Monta_Matriz_Global()
   call Impoe_Condicoes_de_Contorno()
   call Sistema_Linear_Gauss(neq,RigGlobal,Carga,Desloc,io)
   call Calcula_Incognitas_Secundarias()
   call Imprime_Resultados()
   call Resultados_GiD()
   call Fecha_Arquivos()

end program trelica95

