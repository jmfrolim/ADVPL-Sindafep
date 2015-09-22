/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Ponto de Entrada										 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Cadastro de Clientes									 !
+------------------+---------------------------------------------------------+
!Nome 			   ! SINDAFEP-PE-MA030BUT									 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para inclusão de ítem no menu Ações Relacionadas !
!				   ! no cadastro de clientes								 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 23/12/2014												 !
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
! Programa 	! MA030BUT	! Autor !Gilson Lima 		 ! Data ! 23/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Adiciona ítem no menu Ações Relacionadas				 		 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function MA030BUT()

	Local aButtons := {} // Botoes a adicionar
	
	aAdd(aButtons,{'BUDGETY',{|| U_FAltFil()},'Fiscal - Filiação e Desfiliação','Fiscal - Filiação'})
	aAdd(aButtons,{'BUDGETY',{|| U_ExbHFil()},'Fiscal - Histórico de Filiação','Fiscal - Hist. Filiação'})

	// Relatórios de autorização bancária
	// 341 – Itaú, 104 – CEF, 001 – Banco do Brasil.
	aAdd(aButtons,{'BUDGETY',{|| U_RFAT001()},'Autorização de Débito Bancário BB','Aut. Deb. Banc. BB'})
	aAdd(aButtons,{'BUDGETY',{|| U_RFAT002()},'Autorização de Débito Bancário CEF','Aut. Deb. Banc. CEF'})
	
Return (aButtons )