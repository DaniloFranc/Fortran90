program vetor
implicit none
!Programa vetor criado em 03/10/2014 
!serve para imprimir um vetor A(implicito dentro do programa) de modo
!desordenado(como ele foi declarado dentro do programa),crescente e decrescente  
!(a limita��o do programa � que o usuario n�o tem liberdade de escolher o vetor
!a ser ordenado, 
!e tambem que os valores n�o v�o ser impressos lado a lado(em matriz)
!talvez dificultando a leitura do vetor 
!desodenado, crescente e decrescente pelo usuario)


!declara��o de variaveis inteiras
!(por que o vetor vai ser declarado abaixo com valores inteiros)
!uma de dimens�o 11 de nome A, uma auxiliar de nome aux para armazenar dados
!e as outras para servir de contadores i e j.
	
integer, dimension(11) :: A  
integer :: i,j,aux

A(1) = 1; A(2) = 211; A(3) = 979; A(4) = 31; A(5) = 93; A(6) = 327
A(7) = 7; A(8) = 35; A(9) = 56; A(10) = 273; A(11) = 527 

!Vetor como ele foi declarado, desordenadamente

!imprime o vetor de modo desordenado, fazendo de i=1 at� 11 (tamanho do vetor)
!que ele imprima cada elemento do vetor
	
write(*,*)"    Desordenado!"

do i=1,11 !faz i=1 at� 11

write(*,*) A(i) !imprime um elemento i do vetor

end do




!Ordem Crescente

!	A ideia deste comando do em i depois em j � comparar posi��es do vetor 
!de modo consecutivo, exemplo: primeiro toma i=1 e j=2 comparando a 
!primeira posi��o do vetor a com a segunda, se o elemento da primeira posi��o 
!for maior que o da segunda, armazena-se o este valor na variavel aux, 
!passa-se o elemento da segunda posi��o para a primeira e coloca-se o 
!elemento da primeira que foi armazenado em aux na segunda posi��o. 
!Feito isto ele compara ainda em i=1 agora passando para j=3 e repete todo o
!processo at� j=11 (ultimo elemento do vetor a), chegando em j=11 ele muda o 
!valor de i passando de i=1 para i=2 e assim sucessivamente at� i=10.
!i � no maximo 10 por que a ideia deste comando do conjunto(em i e j)
!� passar o maior valor para a ultima posi��o e como a 
!ultima posi��o � 11(tamanho do vetor) n�o podemos colocar i at� 11 
!pois ele iria passar o maior valor para a posi��o 12, que n�o existe!
!esta ideia acima vai ser usada para ordenar o vetor de modo decrescente,
!s� que mudando o sinal na hora de compararmos os elementos i e j 


write(*,*)"     Crescente!" 

do i=1, 10  !faz i = 1 at� 10

	do j = i+1, 11 !faz j = i+1 at� 11 (tamanho do vetor)  
	
	if (A(i) > A(j)) then !compara um elemento com outro concecutivo
	aux = A(i) !se A(i) for maior que A(j) armazena-se A(i) em aux 
	A(i) = A(j) !substitui o elemento seguinte no anterior 	
	A(j) = aux !substitui o maior elemento no consecutivo,
			!para comparar no proximo passo  
		end if
	end do
end do 


	
	!o comando do abaixo � para simplesmente imprimir 
	!os valores do vetor A, 
	!agora ordenado crescentemente  

		do i =1, 11 
			write(*,*) A(i) 
			end do






!Ordem Decrescente

!Usamos a ideia acima comparando elementos i e i+1=j, se o elemento i for menor
!armazena-se este elemento na variavel auxiliar(aux), substitui-se o elemento
!j=i+1 para o elemento i e retorna o elemento i(armazenado em aux) no elemento j
!ou seja faz-se uma troca de elementos se i for menor.

write(*,*)"    Decrescente!"

do i=1, 10 !faz i=1 at� 10
	do j = i+1, 11 !faz j = i+1 at� 11(tamanho do vetor)
		if (A(i) < A(j)) then !compara A(i) com A(j)
			aux = A(i) !armazena-se A(i) em aux
			A(i) = A(j) !substitui-se A(j) em A(i) 
			A(j) = aux !substitui-se A(i)(armazenado em aux) em A(j)
			!feito isto o menor est� na posi��o i+1 com isto 
			!compara-se com o outro concecutivo de modo que 
			!no proximo passo se ele for o menor 
			!ele � jogado l� na frente
		end if
	end do
end do 
	


	!o comando do abaixo � para simplesmente imprimir 
	!os valores do vetor A, 
	!agora ordenado decrescentemente 
		
		do i =1, 11 
write(*,*) A(i) 
end do


stop
end program vetor