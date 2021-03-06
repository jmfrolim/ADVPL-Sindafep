/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manuten��o 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Financeiro												 !
+------------------+---------------------------------------------------------+
!Nome 			   ! FIN-PLANOS-ATUALNOME 									 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a Aglutina��o do Contas a Receber dos Planos!
!				   ! de Sa�de o Odontol�gicos								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 06/04/2015												 !
+------------------+---------------------------------------------------------+
! ATUALIZACOES 	   															 !
+-------------------------------------------+-----------+-----------+--------+
! Descricao detalhada da atualizacao 		!Nome do    ! Analista  !Data da !
! 											!Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
! 											! 			! 			! 		 !
! 											! 			! 			! 		 !
+-------------------------------------------+-----------+-----------+--------*/

#Include "Protheus.ch"

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! PlnAtNm	! Autor !Gilson Lima 		 ! Data ! 13/02/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao atualiza nomes na tabela SZJ, baseada no C�digo de 	 !
!           ! parentesco e Loja. 													 !
+----------------------------------------------------------------------------*/
User Function PlAtNmF()

	Local cNome := ''
	Local nRegL	:= 0
	Local nRegA := 0
	
	dbSelectArea("SZH")
	SZH->(dbSetOrder(1))
	SZH->(dbGoTop())
	
	While SZH->(!EOF())
		
		cNome := POSICIONE("SA1",1,xFilial("SA1")+SZH->ZH_CODTIT+SZH->ZH_LOJA,"A1_NOME")
		
		If AllTrim(cNome) != ''
		
			RecLock("SZH",.F.)
			
				SZH->ZH_NOMETIT := cNome
				
			MsUnLock()
		EndIf
		
		SZH->(dbSkip())
	End
	
	Alert("Conclu�do: Foram lidos " + cValToChar(nRegL) + " registros na SZJ e efetuada " + cValToChar(nRegA) + " altera��es.")

Return