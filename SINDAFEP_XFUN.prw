/*
+----------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              !                                                         !
+------------------+---------------------------------------------------------+
!Modulo            !                                                         !
+------------------+---------------------------------------------------------+
!Nome              ! XFUN                                                    !
+------------------+---------------------------------------------------------+
!Descricao         !                                                         !
!                  !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Marcos Vinicius Perusselo                               !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 15/04/2014                                              !
+------------------+---------------------------------------------------------+
!   ATUALIZACOES                                                             !
+-------------------------------------------+-----------+-----------+--------+
!   Descricao detalhada da atualizacao      !Nome do    ! Analista  !Data da !
!                                           !Solicitante! Respons.  !Atualiz.!
+-------------------------------------------+-----------+-----------+--------+
!                                           !           !           !        !
!                                           !           !           !        !
+-------------------------------------------+-----------+-----------+--------+
*/

#INCLUDE "PROTHEUS.CH"
#INCLUDE "RWMAKE.CH"

#DEFINE CRLF CHR(13) + CHR(10)

User Function fGManuPl(nOrig)
	/*
	Desc.: Campo que esta chamando o Gatilho.
	nOrig = 1 ; ZJ_CODPAR
	nOrig = 2 ; ZJ_CODIGO
	*/
	
	Local cCodPar := aCOLS[N, GDFieldPos("ZJ_CODPAR")]
	Local cCodTit := IIF(nOrig == 1, M->ZH_CODTIT, aCOLS[N, GDFieldPos("ZJ_CODIGO")])
	
	aCOLS[N, GDFieldPos("ZJ_DESCPAR")] := Posicione("SZI", 1, xFilial("SZI")+cCodPar, "ZI_DESCR")
	
	// Titular
	If (cCodPar == '000000')
		aCOLS[N, GDFieldPos("ZJ_CODIGO")] := cCodTit
		aCOLS[N, GDFieldPos("ZJ_NOME")] := Posicione("SA1", 1, xFilial("SA1")+M->ZH_CODTIT, "A1_NOME")
		aCOLS[N, GDFieldPos("ZJ_LOJA")] := M->ZH_LOJA
		aCOLS[N, GDFieldPos("ZJ_DTNASC")] := Posicione("SA1", 1, xFilial("SA1")+M->ZH_CODTIT+M->ZH_LOJA, "A1_DTNASC")
		aCOLS[N, GDFieldPos("ZJ_IDADE")] := IIF(aCOLS[N, GDFieldPos("ZJ_DTNASC")] != CtoD("  /  /    "), U_CALCIDADE(aCOLS[N, GDFieldPos("ZJ_DTNASC")]), CtoD("  /  /    "))
		aCOLS[N, GDFieldPos("ZJ_SEXO")] := Posicione("SA1", 1, xFilial("SA1")+M->ZH_CODTIT+M->ZH_LOJA, "A1_SEXO")
		aCOLS[N, GDFieldPos("ZJ_CPF")] := Posicione("SA1", 1, xFilial("SA1")+M->ZH_CODTIT+M->ZH_LOJA, "A1_CGC")
		aCOLS[N, GDFieldPos("ZJ_RG")] := Posicione("SA1", 1, xFilial("SA1")+M->ZH_CODTIT+M->ZH_LOJA, "A1_PFISICA")
		aCOLS[N, GDFieldPos("ZJ_NOMEMAE")] := Posicione("SA1", 1, xFilial("SA1")+M->ZH_CODTIT+M->ZH_LOJA, "A1_NOMAE")
		//aCOLS[N, GDFieldPos("ZJ_CODPLAN")] := ""
		aCOLS[N, GDFieldPos("ZJ_DESPLAN")] := " "
		aCOLS[N, GDFieldPos("ZJ_INC24H")] := " "
		aCOLS[N, GDFieldPos("ZJ_DATATER")] := CtoD("  /  /    ")
	ElseIf (cCodPar != '000000' .AND. nOrig == 1)
		//aCOLS[N, GDFieldPos("ZJ_CODIGO")] := ""
		aCOLS[N, GDFieldPos("ZJ_NOME")] := ""
		aCOLS[N, GDFieldPos("ZJ_LOJA")] := ""
		aCOLS[N, GDFieldPos("ZJ_REGNASC")] := ""
		aCOLS[N, GDFieldPos("ZJ_DTNASC")] := ""
		aCOLS[N, GDFieldPos("ZJ_IDADE")] := ""
		aCOLS[N, GDFieldPos("ZJ_SEXO")] := ""
		aCOLS[N, GDFieldPos("ZJ_CPF")] := ""
		aCOLS[N, GDFieldPos("ZJ_RG")] := ""
		aCOLS[N, GDFieldPos("ZJ_NOMEMAE")] := ""
		//aCOLS[N, GDFieldPos("ZJ_CODPLAN")] := ""
		aCOLS[N, GDFieldPos("ZJ_DESPLAN")] := " "
		aCOLS[N, GDFieldPos("ZJ_INC24H")] := " "
		aCOLS[N, GDFieldPos("ZJ_DATATER")] := CtoD("  /  /    ")
		aCOLS[N, GDFieldPos("ZJ_FAIXAET")] := " "
	Else
		aCOLS[N, GDFieldPos("ZJ_CODIGO")] := cCodTit
		aCOLS[N, GDFieldPos("ZJ_NOME")] := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_NOME")
		aCOLS[N, GDFieldPos("ZJ_LOJA")] := ""
		aCOLS[N, GDFieldPos("ZJ_REGNASC")] := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_REGNASC")
		aCOLS[N, GDFieldPos("ZJ_DTNASC")] := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_DTNASC")
		aCOLS[N, GDFieldPos("ZJ_IDADE")] := IIF(aCOLS[N, GDFieldPos("ZJ_DTNASC")] != CtoD("  /  /    "), U_CALCIDADE(aCOLS[N, GDFieldPos("ZJ_DTNASC")]), CtoD("  /  /    "))
		aCOLS[N, GDFieldPos("ZJ_SEXO")] := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_SEXO")
		aCOLS[N, GDFieldPos("ZJ_CPF")] := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_CPF")
		aCOLS[N, GDFieldPos("ZJ_RG")] := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_RG")
		aCOLS[N, GDFieldPos("ZJ_NOMEMAE")] := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_NOMEMAE")
		//aCOLS[N, GDFieldPos("ZJ_CODPLAN")] := ""
		aCOLS[N, GDFieldPos("ZJ_DESPLAN")] := " "
		aCOLS[N, GDFieldPos("ZJ_INC24H")] := " "
		aCOLS[N, GDFieldPos("ZJ_DATATER")] := CtoD("  /  /    ")
	EndIf
Return IIF(nOrig == 1, cCodPar, aCOLS[N, GDFieldPos("ZJ_CODIGO")])

/*

*/
User Function fGValVirt(cCampo)
	/*
	Desc.: Campo que esta chamando o Gatilho para preenchimento do campo virtual.
	*/
	
	Local cCodPar := SZJ->ZJ_CODPAR //aCOLS[N, GDFieldPos("ZJ_CODPAR")]
	Local cCodTit := SZJ->ZJ_CODIGO //aCOLS[N, GDFieldPos("ZJ_CODIGO")]
	Local cCodLoj := SZJ->ZJ_LOJA //aCOLS[N, GDFieldPos("ZJ_LOJA")]
	Local cRet := ""
	
	Do Case
		Case (cCampo == "ZJ_SEXO")
			If (cCodPar == '000000')
				cRet := Posicione("SA1", 1, xFilial("SA1")+cCodTit+cCodLoj, "A1_SEXO")
			Else
				cRet := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_SEXO")
			EndIf
		Case (cCampo == "ZJ_CPF")
			If (cCodPar == '000000')
				cRet := Posicione("SA1", 1, xFilial("SA1")+cCodTit+cCodLoj, "A1_CGC")
			Else
				cRet := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_CPF")
			EndIf
		Case (cCampo == "ZJ_RG")
			If (cCodPar == '000000')
				cRet := Posicione("SA1", 1, xFilial("SA1")+cCodTit+cCodLoj, "A1_PFISICA")
			Else
				cRet := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_RG")
			EndIf
		Case (cCampo == "ZJ_NOMEMAE")
			If (cCodPar == '000000')
				cRet := Posicione("SA1", 1, xFilial("SA1")+cCodTit+cCodLoj, "A1_NOMAE")
			Else
				cRet := Posicione("SZG", 1, xFilial("SZJ")+cCodTit, "ZG_NOMEMAE")
			EndIf
	EndCase
Return cRet