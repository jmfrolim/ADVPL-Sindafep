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
!Nome 			   ! IMP-CADFUNC 											 !
+------------------+---------------------------------------------------------+
!Descricao 		   ! Rotina para a importação da base de funcionários para	 !
!				   ! a tabela de clientes									 !
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
! Programa 	! IMPFUNC01	! Autor !Gilson Lima 		 ! Data ! 04/12/2014     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Importa os funcionários para a tabela de clientes				 !									 !
! 			! 																 !
+----------------------------------------------------------------------------*/
User Function IMPFUNC01()

	Local aCliente	:= {}
	Local cCodigo	:= ''
	Local cLoja		:= ''
	Local cPessoa	:= '' 		
	Local cTipo		:= ''
	Local cMatFunc	:= ''
	Local cEstCivil	:= '7'
	Local cGrpVen	:= "000003" // Funcionário
	Local nOpc		:= 0

	Private lMsErroAuto := .F. // Determina se houve alguma inconsistencia na execucao da rotina
	
	dbSelectArea("SRA")
	SRA->(dbSetOrder(1))
	SRA->(dbGoTop())
	
	While SRA->(!EOF())

		lMsErroAuto	:= .F.
		aCliente	:= {}
		
		cMatFunc	:= POSICIONE("SA1",11,xFilial("SA1")+SRA->RA_MAT,"A1_MATFUNC")
	
		// Verifica se é inclusão ou alteração
		If AllTrim(cMatFunc) != ''
			nOpc	:= 4 		//3- InTOTVSclusão, 4- Alteração, 5- Exclusão
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
			Case SRA->RA_ESTCIVI == "C"	// Casado(a)
				cEstCivil := '1'
			Case SRA->RA_ESTCIVI == "M"	// União Estável
				cEstCivil := '2'
			Case SRA->RA_ESTCIVI == "D"	// Divorciado(a)
				cEstCivil := '3'
			Case SRA->RA_ESTCIVI == "Q"	// Separado(a)
				cEstCivil := '4'
			Case SRA->RA_ESTCIVI == "S"	// Solteiro(a)
				cEstCivil := '5'
			Case SRA->RA_ESTCIVI == "V"	// Viuvo(a)
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
		aAdd(aCliente,{"A1_MATFUNC",SRA->RA_MAT,Nil})
		aAdd(aCliente,{"A1_NOME",SRA->RA_NOME,Nil})
		aAdd(aCliente,{"A1_NREDUZ",Substr(SRA->RA_NOME,1,20),Nil})
		aAdd(aCliente,{"A1_CGC",SRA->RA_CIC,Nil})
		aAdd(aCliente,{"A1_PFISICA",SRA->RA_RG,Nil})
		aAdd(aCliente,{"A1_END",AllTrim(SRA->RA_ENDEREC)+", "+Alltrim(SRA->RA_NUMENDE),Nil})
		aAdd(aCliente,{"A1_COMPLEM",SRA->RA_COMPLEM,Nil})
		aAdd(aCliente,{"A1_BAIRRO",SRA->RA_BAIRRO,Nil})
		aAdd(aCliente,{"A1_MUN",SRA->RA_MUNICIP,Nil})
		aAdd(aCliente,{"A1_EST",SRA->RA_ESTADO,Nil})
		aAdd(aCliente,{"A1_CEP",SRA->RA_CEP,Nil})
		aAdd(aCliente,{"A1_DDD",Substr(SRA->RA_DDDFONE,1,3),Nil})
		aAdd(aCliente,{"A1_TEL",Substr(SRA->RA_TELEFON,1,15),Nil})
		aAdd(aCliente,{"A1_XCELUL",Substr(SRA->RA_NUMCELU,1,10),Nil})
		aAdd(aCliente,{"A1_NOMAE",SRA->RA_MAE,Nil})
		aAdd(aCliente,{"A1_NOPAI",SRA->RA_PAI,Nil})
		aAdd(aCliente,{"A1_XDESTCI",cEstCivil,Nil})
		aAdd(aCliente,{"A1_SEXO",SRA->RA_SEXO,Nil})
		aAdd(aCliente,{"A1_DTNASC",SRA->RA_NASC,Nil})
		aAdd(aCliente,{"A1_BITMAP",SRA->RA_BITMAP,Nil})
		aAdd(aCliente,{"A1_EMAIL",Substr(SRA->RA_EMAIL,1,30),Nil})
		
		MSExecAuto({|x,y| Mata030(x,y)},aCliente,nOpc) //3- Inclusão, 4- Alteração, 5- Exclusão
		
		If lMsErroAuto
			MostraErro()
			Return()
		Else
			If nOpc == 3
				ConfirmSX8()
			EndIf
		EndIf
		
		SRA->(dbSkip())
		
	End
	
	MsgInfo("Importação de Funcionários efetuada com sucesso!","Importação de Funcionários")
	
Return()