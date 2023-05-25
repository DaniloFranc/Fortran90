!O modelo de ising � basicamente um modelo para simular os comportamentos
!termodin�micos do meu sistema neste caso vou simular uma rede NxN e saber
!qual a previs�o do meu calor especifico, sucetibilidade magn�tica,
!energia interna e magnetiza��o desta rede NxN. as limite��es deste programa s�o
!dependendo da rede e do numeros de passos demora muito para executar,
!o gerador de numeros semi-aleatorios gera alguns numeros repetidos gerando ruidos
!no que seria as medidas simuladas em fun��o da temperatura.
	
	
	!Aqui come�ei o programa e declarei as vari�veis, algumas foi usado o
!doble precision para que sejam eliminados os erros de algarismos significativos
!e para obtermos uma presi��o maior assim simulando um sistema mais pr�ximo
!da realidade.
	
	
program monte
implicit none
double precision :: ranf
double precision :: T, H, M, E, J, K, deltaM, deltaE
double precision :: SE, SE2, SM, SM2, PB, X, E_media, M_media
double precision :: E_media_quadrado, M_media_quadrado, C
integer :: i, jj, PM, PMT, PMD, L, P, N, NP
integer, allocatable, DIMENSION(:,:) :: sigma

!na variavel ranf foi armazenada os meus numeros aleat�rios, foi atribuira a ela
!o valor zero para que n�o misturem os numeros aleat�rios com os ruidos que ela
!possa estar guardando gerando conflitos. H = 0 pois foi pedido para que o campo
!fosse zero durante a simula��o. o campo n�o � atribuido pelo usuario
!durante a execu��o do programa

ranf = 0
H = 0

!Aqui o usuario escolhe o numero de passos PMT e o numero de passos descartados 
!PMD


	write(*,*) "Campo magnetico", H
	write(*,*) "Digite o tamanho da rede N"
	read(*,*) N
	write(*,*) "Digite o numero de passos de Monte Carlo PMT"
	read(*,*) PMT
	write(*,*) "Digite o numero de passos desprezados PMD"
	read(*,*) PMD
	
!foi atribuido o numero NP ao tamanho da minha rede no caso NxN	


NP = N*N


!Define o tamanho da matriz sigma para depois colocar inicialmente todos os 
!spins para cima 
!(neste caso inicialmente todos os spins foram colocados para cima 
!para depois simular a intera��o entre eles)
	
	
ALLOCATE(sigma(0:N+1,0:N+1))

!declara��o da matriz sigma como todos spins "up" para cima igual 1.

	do i=1, N
		do jj=1, N
			sigma(i,jj)=1
			
		end do
		
	end do
	
!Aplicando as condi��es de contorno da rede quadrada na matriz sigma

	do i=1, N
	sigma(i,N+1)=sigma(i,1)
	sigma(i,0)=sigma(i,N)
	sigma(N+1,i)=sigma(1,i)
	sigma(0,i)=sigma(N,i)
	end do
	
!Atibuindo os valores iniciais de E e M 
!para que possamos dar passos 
!e para cada passo dar um deltaM ou deltaE 
!de acordo com troca de spin de "up" para "down"	

	
M=NP
E= -2*NP

!Criando o arquivo atibuindo a ele o valor 1 e toda vez que eu me referir a ele 
!seja no comando write ou OPEN coloco 1,comando que desejar.
!inprimindo no meu arquivo com o write
!o topo da minha tabela onde defino quem � o que nas minhas "medidas"


OPEN(1,FILE='saida.dat')

!Formata��o de texto de modo que o titulo(da minha tabela) que est� abaixo seja 
!impresso em forma de tabela e para facil exporta��o para um programa de graficos

write(1,20)  'T', 'E_media/NP', 'C/NP', 'M_media/NP', 'X/NP'

!Formata��o de texto de modo que as vari�veis sejam formatadas
!ordenadamente como tabela no meu arquivo 
	
20 FORMAT(5(a18,x))

!defino T = 0.01 para depois dar os passos no do while de at� a temperatura 6

T = 0.01

!Criando minha tabela dentro do meu arquivo os valores para cada temperatura de
!0.01 at� T<6, seria at� 5.91 j� que T varia de 0.10
	
do while (T<6)

!declarando as variaveis SE,SM,SE2 e SM2 zero para come�ar os passos do zero
!os passos segintes ser�o dados da sequinte forma: 
!ele varre a rede de spins todos "up" quando ele ver 
!que PB (que � o peso da fun��o) � maior que o numero semi-aleat�rio ele faz um "flip"
!ou seja, ela muda o spin de "up" para "down" da� ele atribui um deltaE e um deltaM
!associado a varia��o de energia e magnetiza��o da troca de spin (de +1 para -1)
!aplica-se as condi��es de contorno novamente e est� terminado um passo.
!feito isso com todos os spins possivelmente "flip�veis"
!ele varre a rede de novo e d� um novo passo descrito acima repetindo-o. 
	
		SE=0
		SM=0
		SE2=0
		SM2=0

!Comando do de PM=1 at� PMT que � numerod e passos informado pelo usu�io
		
	do PM=1, PMT
	      
!aqui um comando do de L=1 at� N e 
!um de P=1 at� N pois queremos varrer a rede inteira
		
	   	   do L = 1, N
		
			do P= 1, N
			
!o comando CALL random_number atribui um numero "aleat�io" a vari�vel ranf
!coloquei "" em aleat�rio pois s� seria aleot�rio de fato,
!se n�o gerasse os mesmo numeros.


!para cada passo em P ele atribui um valor para ranf da� compara com o peso PB
			
				CALL random_number(ranf)
				
!Ao trocar um spin de +1 para -1 (chamamos isto de flipe) o programa calcula
!Um deltaM e um deltaE 
							
deltaM= -2*sigma(L,P)
deltaE= -deltaM*(sigma(L-1,P) + sigma(L,P+1) + sigma(L+1,P) + sigma(L,P-1))
	
!Aqui ele aplica as condi��es de contorno novamente para a rede 

	if (deltaE <= 0) then
		sigma(L,P)= -sigma(L,P)f
		
	if (L==1) sigma(N+1,P) = sigma(L,P)
	if (L==N) sigma(0,P) = sigma(L,P)
	if (P==1) sigma(L,N+1) = sigma(L,P)
	if (P==N) sigma(L,0) = sigma(L,P)
	
	else
	
!PB � a fun��o peso que serve de paramentro para 
!calcular qual spin deve ser flipado na rede 

	PB = dexp((-deltaE/T))
		
!Se a fun��o peso for maior ou igual o numero aleat�rio o spin correspondente a
!L , P deve ser trocado de +1 para -1 
	
	if (PB >= ranf) then
		sigma(L,P) = -sigma(L,P)
	
	if (L==1) sigma(N+1,P) = sigma(L,P)
	if (L==N) sigma(0,P) = sigma(L,P)
	if (P==1) sigma(L,N+1) = sigma(L,P)
	if (P==N) sigma(L,0) = sigma(L,P)
	
	else
	
!Sen�o � atribuido valor zero as varia��es de energia e magnetiza��o
		
		deltaE=0
		deltaM=0
	
	end if
		end if
		
!toda vez que h� um flipe como foi dito h� uma varia��o de energia e magnetiza��o
!esta varia��o � acrescentada no valor inicial de E e M para mais tarde fazer os calculos
		
		E = E + deltaE
		M = M + deltaM
		
		
		end do
	end do

		
!para os calculos de energia,magnetiza��o,calor especifico e susceptibilidade m�dias
!s�o feitos depois que o numero de passos de Monte Carlo PM for maior que 
!o numero de passos descartados PMD. 
!Pois os passos iniciais servem para "aquecer" o sistema.
!Para isso usamos os valores SE,SM,SE2 e SM2 que no final � sempre acrescentado
!E,M,E� e M� respectivamente.
		
	if (PM > PMD) then
		SE = SE + E
		SM = SM + M
		SE2 = SE2 + E*E
		SM2 = SM2 + M*M
	end if

	
	
end do

!estes s�o os calculos de Energia m�dia, Calor especifico, Magnetiza��o m�dia e
!Susceptibilidade m�dias do nosso sistema


E_media = SE/(PMT - PMD)

E_media_quadrado =  SE2/(PMT - PMD)

C = (E_media_quadrado - E_media**2)/T*T

M_media = SM/(PMT - PMD)

M_media_quadrado = SM2/(PMT - PMD)

X = (1/T)*(M_media_quadrado - M_media**2)

!Aqui � escrito no arquivo criado (valor que se refere ao arquivo � 1 no caso)
!Da� � escrito no arquivo os valores das vari�veis T, E_media/NP, C/NP, M_media/NP e X/NP.
	
	
write(1,10) T, E_media/NP, C/NP, M_media/NP, X/NP

!Outra vez a formata��o de texto para imprimir como tabela	

10 FORMAT(5(F18.5,X))

!Aqui d�-se o passo de temperatura  de 0.1
!(ou seja acrescenta-se 0.1 a temperatura para fazer o calculo no passo sequinte)
	
	T = T + 0.1

end do

!Aqui fora do camando do while de (T < 6) fecha-se o arquivo finalizando-o 
	
	close(UNIT = 1)
	
!Desaloca a vari�vel sigma n�o extinguindo mas desocupando a memoria que ela ocupou
	
DEALLOCATE(sigma)

!Fim do programa

stop
end program
