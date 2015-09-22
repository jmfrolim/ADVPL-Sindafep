/*
+----------------------------------------------------------------------------+
!                         FICHA TECNICA DO PROGRAMA                          !
+----------------------------------------------------------------------------+
!   DADOS DO PROGRAMA                                                        !
+------------------+---------------------------------------------------------+
!Tipo              ! Relatorio                                               !
+------------------+---------------------------------------------------------+
!Modulo            ! FAT                                                     !
+------------------+---------------------------------------------------------+
!Nome              ! RRECIB000                                               !
+------------------+---------------------------------------------------------+
!Descricao         ! Recibo de Titulos. 									 !
!                  !                                                         !
+------------------+---------------------------------------------------------+
!Autor             ! Gilson M. Lima				                             !
+------------------+---------------------------------------------------------+
!Data de Criacao   ! 26/01/2015                                              !
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

#define DMPAPER_A4 9

/*----------+-----------+-------+--------------------+------+----------------+
! Programa 	! RRECIB000	! Autor !Gilson Lima 		 ! Data ! 26/01/2015     !
+-----------+-----------+-------+--------------------+------+----------------+
! Descricao ! Funcao chamada no menu para a geração do recibo				 !
! 			!  																 !
+----------------------------------------------------------------------------*/
User Function RRECIB000()
	
	Private cPerg		:= "RRECIB"
	Private aPergs		:= {}
	
	// Cria pergunta caso não exista
	SX1->(dbSeek(xFilial("SX1")+cPerg,.T.))
	If SX1->(!Found())

		AADD(aPergs,{"Série","","","mv_ch1","C",1,0,0,"C","","MV_PAR01","A","","","","","B","","","","","C","","","","","D","","","","","","","","","","","","",""})
		AADD(aPergs,{"Banco","","","mv_ch2","C",4,0,0,"G","","MV_PAR02","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Cheque","","","mv_ch3","C",10,0,0,"G","","MV_PAR03","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})

		AADD(aPergs,{"Descrição","","","mv_ch4","C",60,0,0,"G","","MV_PAR04","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Descrição","","","mv_ch5","C",60,0,0,"G","","MV_PAR05","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Descrição","","","mv_ch6","C",60,0,0,"G","","MV_PAR06","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Descrição","","","mv_ch7","C",60,0,0,"G","","MV_PAR07","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AADD(aPergs,{"Descrição","","","mv_ch8","C",60,0,0,"G","","MV_PAR08","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""})
		AjustaSx1(cPerg,aPergs)
	EndIf
	
	// Carrega Perguntas
	Pergunte(cPerg,.T.)

	Processa({|| RRecib001()},"Gerando Recibo")

Return

Static Function RRECIB001()

	Local oPrint := Nil
	
	Private	cSerie	:= MV_PAR01
	Private cBanco	:= MV_PAR02
	Private cCheque	:= MV_PAR03

	Private cDescr1	:= MV_PAR04
	Private cDescr2	:= MV_PAR05
	Private cDescr3	:= MV_PAR06
	Private cDescr4	:= MV_PAR07
	Private cDescr5	:= MV_PAR08
	
	Private cSerRec	:= "A"
	
	Do Case
		Case MV_PAR01 == 1
			cSerRec := 'A'
		Case MV_PAR01 == 2
			cSerRec := 'B'
		Case MV_PAR01 == 3
			cSerRec := 'C'
		Case MV_PAR01 == 4
			cSerRec := 'D'
		Otherwise
			cSerRec	:= 'A'
	EndCase
	
	Private cNrRec	:= GeraNr(cSerRec)
	
	Private cTitHist := AllTrim(SE1->E1_PREFIXO) + " " + AllTrim(SE1->E1_NUM) + " | " + AllTrim(SE1->E1_HIST)
	Private cTitVenc := DtoC(SE1->E1_VENCREA)
	Private cTitVal	 := Transform(SE1->E1_VALOR,'@E 999,999,999.99')
	
	Private cNaturez := AllTrim(Posicione("SED",1,xFilial("SED")+SE1->E1_NATUREZ,"ED_DESCRIC"))
	
	Private cCliente := Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_COD")
	Private cGrpVen	 := Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_GRPVEN")
	Private cGrpVenD := AllTrim(Posicione("ACY",1,xFilial("ACY")+cGrpVen,"ACY_DESCRI"))
	Private cNome	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_NOME"))
	Private cEnd	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_END"))
	Private cCompl	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_COMPLEM"))
	Private cBairro	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_BAIRRO"))
	Private cCidade	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_MUN"))
	Private cUF	 	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_EST"))
	Private cTel1 	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_TEL"))
	Private cTel2 	 := AllTrim(Posicione("SA1",1,xFilial("SA1")+SE1->E1_CLIENTE+SE1->E1_LOJA,"A1_XCELUL"))
	
	Private cTel	 := ''
	
	If AllTrim(cTel1) != ''
		cTel += cTel1
	EndIf
	If AllTrim(cTel2) != ''
	 	If Alltrim(cTel) != ''
	 		cTel += " | "
	 	EndIf
	 	cTel += cTel2
	EndIf
	
	Private cEnder	 := cEnd
	If AllTrim(cCompl) != ''
		cEnder += " - " + cCompl
	EndIf
	If AllTrim(cBairro) != ''
		cEnder += " - " + cBairro
	EndIf
	
	Private cCidUF 	  := cCidade
	If AllTrim(cUf) != ''
		If AllTrim(cCidUf)!= ''
			cCidUF += " / "
		EndIf
		cCidUf += cUf
	EndIf
	
	
	oPrint:= TMSPrinter():New("Recibo Sindafep")
	oPrint:Setup()
	oPrint:SetPaperSize(9)
	oPrint:SetPortrait()
	oPrint:StartPage()	

	GeraRecib(@oPrint,70)
	GeraRecib(@oPrint,1170)
	GeraRecib(@oPrint,2280)
	
	oPrint:EndPage()
	oPrint:Preview()
	
Return

Static Function GeraRecib(oPrint,nRow)
	
	Local cLogo :='\system\sindafep.bmp'
	
	Private oFont8		:= TFont():New("Arial",9,8,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont8b		:= TFont():New("Arial",9,8,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont8m		:= TFont():New("Courier New",9,8,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont8mb	:= TFont():New("Courier New",9,8,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9		:= TFont():New("Arial",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9b		:= TFont():New("Arial",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont9m		:= TFont():New("Courier New",9,9,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont9mb	:= TFont():New("Courier New",9,9,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10		:= TFont():New("Arial",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10b	:= TFont():New("Arial",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont10m	:= TFont():New("Courier New",9,10,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont10mb	:= TFont():New("Courier New",9,10,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12		:= TFont():New("Arial",9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont12b	:= TFont():New("Arial",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont12m	:= TFont():New("Courier New",9,12,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont12mb	:= TFont():New("Courier New",9,12,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont14		:= TFont():New("Arial",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont14b	:= TFont():New("Arial",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont14m	:= TFont():New("Courier New",9,14,.T.,.F.,5,.T.,5,.T.,.F.)
	Private oFont14mb	:= TFont():New("Courier New",9,14,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont16b	:= TFont():New("Arial",9,16,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18b	:= TFont():New("Arial",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont18mb	:= TFont():New("Courier New",9,18,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont20mb	:= TFont():New("Courier New",9,20,.T.,.T.,5,.T.,5,.T.,.F.)
	Private oFont24b	:= TFont():New("Arial",9,24,.T.,.T.,5,.T.,5,.T.,.F.)
	
	If (File(cLogo))
		oPrint:SayBitmap(nRow+35,100,cLogo,310,200)
	EndIf

	oPrint:Say(nRow+30,480,"SINDAFEP - Sindicato dos Auditores Fiscais da Receita do Estado do Paraná",oFont10b)
	
	oPrint:Say(nRow+100,480,"SEDE: Rua Alferes Angelo Sampaio, 1793, Batel - Curitiba/PR - CEP 80420-160",oFont8)
	oPrint:Say(nRow+100,1700,"(41) 3221-5300",oFont8)
	
	oPrint:Say(nRow+140,480,"COLÔNIA DE FÉRIAS: Rua Tibagi, 77 - Guaratuba/PR - CEP 83280-000",oFont8)
	oPrint:Say(nRow+140,1700,"(41) 3442-1585",oFont8)
	
	oPrint:Say(nRow+180,480,"HOTAL ROTA DO SOL: Av. Visconde do Rio Branco, 2995 - Guaratuba/PR - CEP 83280-000",oFont8)
	oPrint:Say(nRow+180,1700,"(41) 3443-1313",oFont8)
	
	oPrint:Say(nRow+230,480,"CNPJ: 76.707.686/0001-17",oFont8)
	oPrint:Say(nRow+230,1545,"SITE: www.sindafep.com.br",oFont8)
	
	oPrint:Box(nRow+30,2000,nRow+320,2300)
	
	oPrint:Say(nRow+60,2055,'Série "' + cSerRec + '"',oFont12b)
	oPrint:Say(nRow+140,2056,'RECIBO',oFont12b)
	oPrint:Say(nRow+220,2030,PadL(cNrRec,7,' '),oFont14mb)
	
	oPrint:Box(nRow+320,100,nRow+550,2300)
	
	oPrint:Say(nRow+340,120,'Sócio:',oFont9)
	oPrint:Say(nRow+410,120,'Nome:',oFont9)
	oPrint:Say(nRow+480,120,'Endereço:',oFont9)

	oPrint:Say(nRow+340,300,cGrpVenD,oFont9)
	oPrint:Say(nRow+410,300,cNome,oFont9)
	oPrint:Say(nRow+480,300,cEnder,oFont9)

	oPrint:Say(nRow+340,1520,'Identificação:',oFont9)
	oPrint:Say(nRow+410,1520,'Telefone:',oFont9)
	oPrint:Say(nRow+480,1520,'Cidade/UF:',oFont9)
	
	oPrint:Say(nRow+340,1780,cCliente,oFont9)
	oPrint:Say(nRow+410,1780,cTel,oFont9)
	oPrint:Say(nRow+480,1780,cCidUF,oFont9)
	
	oPrint:Say(nRow+575,120,'NATUREZA DA OPERAÇÃO:',oFont9)
	oPrint:Say(nRow+578,585,cNaturez,oFont9m)
	oPrint:Say(nRow+575,1390,'BANCO',oFont9)
	oPrint:Say(nRow+575,1550,'CHEQUE NR.',oFont9)
	oPrint:Say(nRow+575,1810,'DT. VENCIM.',oFont9)
	oPrint:Say(nRow+575,2100,'VALOR R$',oFont9)

	oPrint:Box(nRow+640,100,nRow+1000,2300)
	
	oPrint:Say(nRow+670,120,cTitHist,oFont9m)
	oPrint:Say(nRow+670,1420,cBanco,oFont9m)
	oPrint:Say(nRow+670,1560,cCheque,oFont9m)
	oPrint:Say(nRow+670,1810,cTitVenc,oFont9m)
	oPrint:Say(nRow+670,1985,cTitVal,oFont9m)
	
	oPrint:Say(nRow+725,120,cDescr1,oFont9m)
	oPrint:Say(nRow+780,120,cDescr2,oFont9m)
	oPrint:Say(nRow+835,120,cDescr3,oFont9m)
	oPrint:Say(nRow+890,120,cDescr4,oFont9m)
	oPrint:Say(nRow+945,120,cDescr5,oFont9m)
	
	oPrint:Box(nRow+1000,100,nRow+1090,2300)
	
	oPrint:Say(nRow+1025,120,'DT. Emissão:',oFont9)
	oPrint:Say(nRow+1025,350,DtoC(dDataBase),oFont9)
	
	oPrint:Line(nRow+1000,750,nRow+1090,750)
	
	oPrint:Say(nRow+1025,800,CUSERNAME,oFont9)

	oPrint:Line(nRow+1000,1750,nRow+1090,1750)
	
	oPrint:Say(nRow+1025,1800,'TOTAL',oFont9)
	oPrint:Say(nRow+1025,1985,cTitVal,oFont9mb)
	
Return

Static Function GeraNr(cSerie)
	
	Local aArea		:= GetArea()
	Local cTabela 	:= 'RN'
	Local cChave  	:= PadR(cSerie,6,' ')
	Local cNum		 := ''
	
	cNum := Posicione("SX5",1,xFilial("SX5")+cTabela+cChave,"X5_DESCRI")
	
	cNum := cValToChar(Val(cNum))
	
	dbSelectArea("SX5")
	SX5->(dbSeek(xFilial("SX5")+cTabela+cChave))
	
		RecLock("SX5",.F.)
			SX5->X5_DESCRI := PadL(Val(cNum)+1,6,'0')
		SX5->(MsUnlock())
	
	SX5->(dbCloseArea())
	
	RestArea(aArea)
	
Return(cNum)