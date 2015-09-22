/*
+----------------------------------------------------------------------------+
!                        FICHA TECNICA DO PROGRAMA                           !
+----------------------------------------------------------------------------+
! DADOS DO PROGRAMA 														 !
+------------------+---------------------------------------------------------+
!Tipo 			   ! Manutenção 											 !
+------------------+---------------------------------------------------------+
!Modulo 		   ! Cadastro de Funcionários								 !
+------------------+---------------------------------------------------------+
!Nome 			   ! PE-CADFUNC 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Ponto de entrada para manutenção do cadastro de		 !
!				   ! funcionários, gravando na tabela clientes os dados		 !
!				   ! inseridos / alterados									 !
+------------------+---------------------------------------------------------+
!Autor 			   ! Gilson Lima		 									 !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 04/12/2014												 !
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
! Programa 	! GP010FIMPE! Autor !Gilson Lima 		 ! Data ! 04/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Ponto de entrada executado após salvar o cadastro de funcioná- !									 !
! 			! rios															 !
+----------------------------------------------------------------------------*/
User Function GP010FIMPE()

	Local aCliente	:= {}
	Local cCodigo	:= ''
	Local cLoja		:= ''
	Local cPessoa	:= '' 		
	Local cTipo		:= ''
	Local cMatFunc	:= POSICIONE("SA1",11,xFilial("SA1")+RA_MAT,"A1_MATFUNC")
	Local cEstCivil	:= '7'
	Local cGrpVen	:= "000003" // Funcionário
	Local nOpc		:= 0
	
	Private lMsErroAuto := .F. // Determina se houve alguma inconsistencia na execucao da rotina
	
	// Verifica se é inclusão ou alteração
	If AllTrim(cMatFunc) != ''
		nOpc	:= 4 		//3- Inclusão, 4- Alteração, 5- Exclusão
		cCodigo	:= SA1->A1_COD
		cLoja	:= SA1->A1_LOJA
		cPessoa	:= SA1->A1_PESSOA
		cTipo	:= SA1->A1_TIPO
	Else
		nOpc	:= 3 		//3- Inclusão, 4- Alteração, 5- Exclusão
		cCodigo	:= GetSXEnum("SA1","A1_COD")
		cLoja	:= '01'
		cPessoa	:= 'F'		// Física
		cTipo	:= "F" 		// Consumidor Final

	EndIf	
	
	// Converte Estado Civil
	Do Case
		Case RA_ESTCIVI == "C"	// Casado(a)
			cEstCivil := '1'
		Case RA_ESTCIVI == "M"	// União Estável
			cEstCivil := '2'
		Case RA_ESTCIVI == "D"	// Divorciado(a)
			cEstCivil := '3'
		Case RA_ESTCIVI == "Q"	// Separado(a)
			cEstCivil := '4'
		Case RA_ESTCIVI == "S"	// Solteiro(a)
			cEstCivil := '5'
		Case RA_ESTCIVI == "V"	// Viuvo(a)
			cEstCivil := '6'
		Otherwise
			cEstCivil := '7'	// Outros - Não informado
	EndCAse
	
	aAdd(aCliente,{"A1_FILIAL",xFilial("SA1"),Nil})
	aAdd(aCliente,{"A1_COD",cCodigo,Nil})
	aAdd(aCliente,{"A1_LOJA",cLoja,Nil})
	aAdd(aCliente,{"A1_PESSOA",cPessoa,Nil})
	aAdd(aCliente,{"A1_GRPVEN",cGrpVen,Nil})
	aAdd(aCliente,{"A1_TIPO",cTipo,Nil})
	aAdd(aCliente,{"A1_MATFUNC",RA_MAT,Nil})
	aAdd(aCliente,{"A1_NOME",RA_NOME,Nil})
	aAdd(aCliente,{"A1_NREDUZ",Substr(RA_NOME,1,20),Nil})
	aAdd(aCliente,{"A1_CGC",RA_CIC,Nil})
	aAdd(aCliente,{"A1_PFISICA",RA_RG,Nil})
	aAdd(aCliente,{"A1_END",AllTrim(RA_ENDEREC)+", "+Alltrim(RA_NUMENDE),Nil})
	aAdd(aCliente,{"A1_COMPLEM",RA_COMPLEM,Nil})
	aAdd(aCliente,{"A1_BAIRRO",RA_BAIRRO,Nil})
	aAdd(aCliente,{"A1_MUN",RA_MUNICIP,Nil})
	aAdd(aCliente,{"A1_EST",RA_ESTADO,Nil})
	aAdd(aCliente,{"A1_CEP",RA_CEP,Nil})
	aAdd(aCliente,{"A1_DDD",Substr(RA_DDDFONE,1,3),Nil})
	aAdd(aCliente,{"A1_TEL",Substr(RA_TELEFON,1,15),Nil})
	aAdd(aCliente,{"A1_XCELUL",Substr(RA_NUMCELU,1,10),Nil})
	aAdd(aCliente,{"A1_NOMAE",RA_MAE,Nil})
	aAdd(aCliente,{"A1_NOPAI",RA_PAI,Nil})
	aAdd(aCliente,{"A1_XDESTCI",cEstCivil,Nil})
	aAdd(aCliente,{"A1_SEXO",RA_SEXO,Nil})
	aAdd(aCliente,{"A1_DTNASC",RA_NASC,Nil})
	aAdd(aCliente,{"A1_BITMAP",RA_BITMAP,Nil})
	aAdd(aCliente,{"A1_EMAIL",Substr(RA_EMAIL,1,30),Nil})

	MSExecAuto({|x,y| Mata030(x,y)},aCliente,nOpc) //3- Inclusão, 4- Alteração, 5- Exclusão
	
	If lMsErroAuto
		MostraErro()
		Return()
	Else
		If nOpc == 3
			ConfirmSX8()
		EndIf
	EndIf
	
Return()