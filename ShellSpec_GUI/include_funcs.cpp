#include "include_funcs.h"
#include "QtSql"
#include "shellspec.h"
#include "QDir"
#include "QFile"
#include "QTextStream"
#include <iostream>
#include <fstream>
#include <sstream>


void include_funcs::create_input_for_shellspec()
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    QString outFile = SS_DIR + "shellspec.in";
    QFile file(outFile);

    shellspec conn;
    conn.connOpen();
    QSqlQueryModel qry;
    qry.setQuery("SELECT * FROM user_inputs WHERE ID=2");
    qDebug()<<"Error creating input for shellspec: "<<qry.lastError()<<endl;

    float alam1 = qry.record(0).value("alam1").toFloat();
    float alamn = qry.record(0).value("alamn").toFloat();
    float alams = qry.record(0).value("alams").toFloat();
    float loglam = qry.record(0).value("loglam").toFloat();
    float cutoff = qry.record(0).value("cutoff").toFloat();
    int imodel = qry.record(0).value("imodel").toInt();
    int irotat = qry.record(0).value("irotat").toInt();
    int ipart = qry.record(0).value("ipart").toInt();
    int ichemc = qry.record(0).value("ichemc").toInt();
    int ielnd = qry.record(0).value("ielnd").toInt();
    int ithom = qry.record(0).value("ithom").toInt();
    int irayl = qry.record(0).value("irayl").toInt();
    int imie = qry.record(0).value("imie").toInt();
    int imiepf = qry.record(0).value("imiepf").toInt();
    int ihyd = qry.record(0).value("ihyd").toInt();
    int iopac = qry.record(0).value("iopac").toInt();
    int iline = qry.record(0).value("iline").toInt();
    float eps = qry.record(0).value("eps").toFloat();
    float ionu = qry.record(0).value("ionu").toFloat();
    float ior = qry.record(0).value("ior").toFloat();
    float iot = qry.record(0).value("iot").toFloat();
    float offset = qry.record(0).value("offset").toFloat();
    float phase1 = qry.record(0).value("phase1").toFloat();
    float phasen = qry.record(0).value("phasen").toFloat();
    int nphase = qry.record(0).value("nphase").toInt();
    float dinc = qry.record(0).value("dinc").toFloat();
    float dd = qry.record(0).value("dd").toFloat();
    int lunt1 = qry.record(0).value("lunt1").toInt();
    float xunt1 = qry.record(0).value("xunt1").toFloat();
    float yunt1 = qry.record(0).value("yunt1").toFloat();
    int lunt2 = qry.record(0).value("lunt2").toInt();
    float xunt2 = qry.record(0).value("xunt2").toFloat();
    float yunt2 = qry.record(0).value("yunt2").toFloat();
    int lunt3 = qry.record(0).value("lunt3").toInt();
    float xunt3 = qry.record(0).value("xunt3").toFloat();
    float yunt3 = qry.record(0).value("yunt3").toFloat();
    float rmdfx1 = qry.record(0).value("rmdfx1").toFloat();
    float rmdfx2 = qry.record(0).value("rmdfx2").toFloat();
    float rmdfy1 = qry.record(0).value("rmdfy1").toFloat();
    float rmdfy2 = qry.record(0).value("rmdfy2").toFloat();
    float rmdfz1 = qry.record(0).value("rmdfz1").toFloat();
    float rmdfz2 = qry.record(0).value("rmdfz2").toFloat();
    float rmdfx3 = qry.record(0).value("rmdfx3").toFloat();
    float rmdfx4 = qry.record(0).value("rmdfx4").toFloat();
    float stepf = qry.record(0).value("stepf").toFloat();
    float stepfz = qry.record(0).value("stepfz").toFloat();
    float gainfx = qry.record(0).value("gainfx").toFloat();
    float gainfy = qry.record(0).value("gainfy").toFloat();
    float gainfz = qry.record(0).value("gainfz").toFloat();
    float rmdx1 = qry.record(0).value("rmdx1").toFloat();
    float rmdx2 = qry.record(0).value("rmdx2").toFloat();
    float rmdy1 = qry.record(0).value("rmdy1").toFloat();
    float rmdy2 = qry.record(0).value("rmdy2").toFloat();
    float rmdz1 = qry.record(0).value("rmdz1").toFloat();
    float rmdz2 = qry.record(0).value("rmdz2").toFloat();
    float rmdz3 = qry.record(0).value("rmdz3").toFloat();
    float rmdz4 = qry.record(0).value("rmdz4").toFloat();
    float steps = qry.record(0).value("steps").toFloat();
    float stepsz = qry.record(0).value("stepsz").toFloat();
    float gainx = qry.record(0).value("gainx").toFloat();
    float gainy = qry.record(0).value("gainy").toFloat();
    float gainz = qry.record(0).value("gainz").toFloat();
    int ISTAR = qry.record(0).value("ISTAR").toInt();
    int ICOMP = qry.record(0).value("ICOMP").toInt();
    int IENV = qry.record(0).value("IENV").toInt();
    int ISPOT = qry.record(0).value("ISPOT").toInt();
    int ISM = qry.record(0).value("ISM").toInt();
    int IRING = qry.record(0).value("IRING").toInt();
    int IDISC = qry.record(0).value("IDISC").toInt();
    int INEBL = qry.record(0).value("INEBL").toInt();
    int IFLOW = qry.record(0).value("IFLOW").toInt();
    int IJET = qry.record(0).value("IJET").toInt();
    int IUFO = qry.record(0).value("IUFO").toInt();
    int ISHELL = qry.record(0).value("ISHELL").toInt();
    float rstar = qry.record(0).value("rstar").toFloat();
    float Tstar = qry.record(0).value("Tstar").toFloat();
    float eMstar = qry.record(0).value("eMstar").toFloat();
    float xstar = qry.record(0).value("xstar").toFloat();
    float ystar = qry.record(0).value("ystar").toFloat();
    float zstar = qry.record(0).value("zstar").toFloat();
    float vrotst = qry.record(0).value("vrotst").toFloat();
    float idifst = qry.record(0).value("idifst").toFloat();
    float drotst = qry.record(0).value("drotst").toFloat();
    float hst = qry.record(0).value("hst").toFloat();
    float vxst = qry.record(0).value("vxst").toFloat();
    float vyst = qry.record(0).value("vyst").toFloat();
    float vzst = qry.record(0).value("vzst").toFloat();
    float dlst = qry.record(0).value("dlst").toFloat();
    float dlst2 = qry.record(0).value("dlst2").toFloat();
    float dgst = qry.record(0).value("dgst").toFloat();
    float ffst = qry.record(0).value("ffst").toFloat();
    int irrst = qry.record(0).value("irrst").toInt();
    int ialbst = qry.record(0).value("ialbst").toInt();
    float albst = qry.record(0).value("albst").toFloat();
    float htst = qry.record(0).value("htst").toFloat();
    float htsta = qry.record(0).value("htsta").toFloat();
    int ispst = qry.record(0).value("ispst").toInt();
    float xspst = qry.record(0).value("xspst").toFloat();
    float yspst = qry.record(0).value("yspst").toFloat();
    float zspst = qry.record(0).value("zspst").toFloat();
    float aspst = qry.record(0).value("aspst").toFloat();
    float tspst = qry.record(0).value("tspst").toFloat();
    float rcp = qry.record(0).value("rcp").toFloat();
    float tempcp = qry.record(0).value("tempcp").toFloat();
    float qq = qry.record(0).value("qq").toFloat();
    float vrxcp = qry.record(0).value("vrxcp").toFloat();
    float vrycp = qry.record(0).value("vrycp").toFloat();
    float vrzcp = qry.record(0).value("vrzcp").toFloat();
    float vrotcp = qry.record(0).value("vrotcp").toFloat();
    float xcp = qry.record(0).value("xcp").toFloat();
    float ycp = qry.record(0).value("ycp").toFloat();
    float zcp = qry.record(0).value("zcp").toFloat();
    float vxcp = qry.record(0).value("vxcp").toFloat();
    float vycp = qry.record(0).value("vycp").toFloat();
    float vzcp = qry.record(0).value("vzcp").toFloat();
    float dlcp = qry.record(0).value("dlcp").toFloat();
    float dlcp2 = qry.record(0).value("dlcp2").toFloat();
    float dgcp = qry.record(0).value("dgcp").toFloat();
    float ffcp = qry.record(0).value("ffcp").toFloat();
    int irrcp = qry.record(0).value("irrcp").toInt();
    int ialbcp = qry.record(0).value("ialbcp").toInt();
    float albcp = qry.record(0).value("albcp").toFloat();
    float htcp = qry.record(0).value("htcp").toFloat();
    float htcpa = qry.record(0).value("htcpa").toFloat();
    float emen = qry.record(0).value("emen").toFloat();
    float qqen = qry.record(0).value("qqen").toFloat();
    float aen = qry.record(0).value("aen").toFloat();
    float ffen = qry.record(0).value("ffen").toFloat();
    float hen = qry.record(0).value("hen").toFloat();
    float tempen = qry.record(0).value("tempen").toFloat();
    float densen = qry.record(0).value("densen").toFloat();
    float aneen = qry.record(0).value("aneen").toFloat();
    float vtrben = qry.record(0).value("vtrben").toFloat();
    float dstden = qry.record(0).value("dstden").toFloat();
    float dstten = qry.record(0).value("dstten").toFloat();
    float vrxsp = qry.record(0).value("vrxsp").toFloat();
    float vrysp = qry.record(0).value("vrysp").toFloat();
    float vrzsp = qry.record(0).value("vrzsp").toFloat();
    float vrotsp = qry.record(0).value("vrotsp").toFloat();
    float rsp = qry.record(0).value("rsp").toFloat();
    float xsp = qry.record(0).value("xsp").toFloat();
    float ysp = qry.record(0).value("ysp").toFloat();
    float zsp = qry.record(0).value("zsp").toFloat();
    float vxsp = qry.record(0).value("vxsp").toFloat();
    float vysp = qry.record(0).value("vysp").toFloat();
    float vzsp = qry.record(0).value("vzsp").toFloat();
    float tempsp = qry.record(0).value("tempsp").toFloat();
    float denssp = qry.record(0).value("denssp").toFloat();
    float anesp = qry.record(0).value("anesp").toFloat();
    float vtrbsp = qry.record(0).value("vtrbsp").toFloat();
    float dstdsp = qry.record(0).value("dstdsp").toFloat();
    float dsttsp = qry.record(0).value("dsttsp").toFloat();
    float v1sm = qry.record(0).value("v1sm").toFloat();
    float v2sm = qry.record(0).value("v2sm").toFloat();
    float r1sm = qry.record(0).value("r1sm").toFloat();
    float r2sm = qry.record(0).value("r2sm").toFloat();
    float x1sm = qry.record(0).value("x1sm").toFloat();
    float y1sm = qry.record(0).value("y1sm").toFloat();
    float z1sm = qry.record(0).value("z1sm").toFloat();
    float x2sm = qry.record(0).value("x2sm").toFloat();
    float y2sm = qry.record(0).value("y2sm").toFloat();
    float z2sm = qry.record(0).value("z2sm").toFloat();
    float vxsm = qry.record(0).value("vxsm").toFloat();
    float vysm = qry.record(0).value("vysm").toFloat();
    float vzsm = qry.record(0).value("vzsm").toFloat();
    float xsm = qry.record(0).value("xsm").toFloat();
    float ysm = qry.record(0).value("ysm").toFloat();
    float zsm = qry.record(0).value("zsm").toFloat();
    float psm = qry.record(0).value("psm").toFloat();
    float tempsm = qry.record(0).value("tempsm").toFloat();
    float denssm = qry.record(0).value("denssm").toFloat();
    float anesm = qry.record(0).value("anesm").toFloat();
    float vtrbsm = qry.record(0).value("vtrbsm").toFloat();
    float edensm = qry.record(0).value("edensm").toFloat();
    float dstdsm = qry.record(0).value("dstdsm").toFloat();
    float dsttsm = qry.record(0).value("dsttsm").toFloat();
    float rrg = qry.record(0).value("rrg").toFloat();
    float emrg = qry.record(0).value("emrg").toFloat();
    float b1rg = qry.record(0).value("b1rg").toFloat();
    float b2rg = qry.record(0).value("b2rg").toFloat();
    float a1rg = qry.record(0).value("a1rg").toFloat();
    float a2rg = qry.record(0).value("a2rg").toFloat();
    float dr1rg = qry.record(0).value("dr1rg").toFloat();
    float dr2rg = qry.record(0).value("dr2rg").toFloat();
    float xrg = qry.record(0).value("xrg").toFloat();
    float yrg = qry.record(0).value("yrg").toFloat();
    float zrg = qry.record(0).value("zrg").toFloat();
    float xpolrg = qry.record(0).value("xpolrg").toFloat();
    float ypolrg = qry.record(0).value("ypolrg").toFloat();
    float zpolrg = qry.record(0).value("zpolrg").toFloat();
    float vxrg = qry.record(0).value("vxrg").toFloat();
    float vyrg = qry.record(0).value("vyrg").toFloat();
    float vzrg = qry.record(0).value("vzrg").toFloat();
    float temprg = qry.record(0).value("temprg").toFloat();
    float densrg = qry.record(0).value("densrg").toFloat();
    float anerg = qry.record(0).value("anerg").toFloat();
    float vtrbrg = qry.record(0).value("vtrbrg").toFloat();
    int itrg = qry.record(0).value("itrg").toInt();
    float edenrg = qry.record(0).value("edenrg").toFloat();
    float dstdrg = qry.record(0).value("dstdrg").toFloat();
    float ede2rg = qry.record(0).value("ede2rg").toFloat();
    float dst2rg = qry.record(0).value("dst2rg").toFloat();
    float dsttrg = qry.record(0).value("dsttrg").toFloat();
    float adisc = qry.record(0).value("adisc").toFloat();
    float rindc = qry.record(0).value("rindc").toFloat();
    float routdc = qry.record(0).value("routdc").toFloat();
    float emdc = qry.record(0).value("emdc").toFloat();
    float rdc = qry.record(0).value("rdc").toFloat();
    float xdc = qry.record(0).value("xdc").toFloat();
    float ydc = qry.record(0).value("ydc").toFloat();
    float zdc = qry.record(0).value("zdc").toFloat();
    float xdisc = qry.record(0).value("xdisc").toFloat();
    float ydisc = qry.record(0).value("ydisc").toFloat();
    float zdisc = qry.record(0).value("zdisc").toFloat();
    float vxdc = qry.record(0).value("vxdc").toFloat();
    float vydc = qry.record(0).value("vydc").toFloat();
    float vzdc = qry.record(0).value("vzdc").toFloat();
    float tempdc = qry.record(0).value("tempdc").toFloat();
    float densdc = qry.record(0).value("densdc").toFloat();
    float anedc = qry.record(0).value("anedc").toFloat();
    float vtrbdc = qry.record(0).value("vtrbdc").toFloat();
    float edendc = qry.record(0).value("edendc").toFloat();
    int itdc = qry.record(0).value("itdc").toInt();
    float etmpdc = qry.record(0).value("etmpdc").toFloat();
    float dstddc = qry.record(0).value("dstddc").toFloat();
    float dsttdc = qry.record(0).value("dsttdc").toFloat();
    float aneb = qry.record(0).value("aneb").toFloat();
    float rinnb = qry.record(0).value("rinnb").toFloat();
    float routnb = qry.record(0).value("routnb").toFloat();
    float emnb = qry.record(0).value("emnb").toFloat();
    float rnb = qry.record(0).value("rnb").toFloat();
    float hinvnb = qry.record(0).value("hinvnb").toFloat();
    float tinvnb = qry.record(0).value("tinvnb").toFloat();
    float hwindnb = qry.record(0).value("hwindnb").toFloat();
    int idennb = qry.record(0).value("idennb").toInt();
    float xneb = qry.record(0).value("xneb").toFloat();
    float yneb = qry.record(0).value("yneb").toFloat();
    float zneb = qry.record(0).value("zneb").toFloat();
    float vxnb = qry.record(0).value("vxnb").toFloat();
    float vynb = qry.record(0).value("vynb").toFloat();
    float vznb = qry.record(0).value("vznb").toFloat();
    float tempnb = qry.record(0).value("tempnb").toFloat();
    float densnb = qry.record(0).value("densnb").toFloat();
    float anenb = qry.record(0).value("anenb").toFloat();
    float vtrbnb = qry.record(0).value("vtrbnb").toFloat();
    float edennb = qry.record(0).value("edennb").toFloat();
    int itnb = qry.record(0).value("itnb").toInt();
    float etmpnb = qry.record(0).value("etmpnb").toFloat();
    float dstdnb = qry.record(0).value("dstdnb").toFloat();
    float dsttnb = qry.record(0).value("dsttnb").toFloat();
    float v1fw = qry.record(0).value("v1fw").toFloat();
    float v2fw = qry.record(0).value("v2fw").toFloat();
    float r1fw = qry.record(0).value("r1fw").toFloat();
    float r2fw = qry.record(0).value("r2fw").toFloat();
    float x1fw = qry.record(0).value("x1fw").toFloat();
    float y1fw = qry.record(0).value("y1fw").toFloat();
    float z1fw = qry.record(0).value("z1fw").toFloat();
    float x2fw = qry.record(0).value("x2fw").toFloat();
    float y2fw = qry.record(0).value("y2fw").toFloat();
    float z2fw = qry.record(0).value("z2fw").toFloat();
    float vxfw = qry.record(0).value("vxfw").toFloat();
    float vyfw = qry.record(0).value("vyfw").toFloat();
    float vzfw = qry.record(0).value("vzfw").toFloat();
    float xfw = qry.record(0).value("xfw").toFloat();
    float yfw = qry.record(0).value("yfw").toFloat();
    float zfw = qry.record(0).value("zfw").toFloat();
    float pfw = qry.record(0).value("pfw").toFloat();
    float tempfw = qry.record(0).value("tempfw").toFloat();
    float densfw = qry.record(0).value("densfw").toFloat();
    float anefw = qry.record(0).value("anefw").toFloat();
    float vtrbfw = qry.record(0).value("vtrbfw").toFloat();
    float edenfw = qry.record(0).value("edenfw").toFloat();
    float dstdfw = qry.record(0).value("dstdfw").toFloat();
    float dsttfw = qry.record(0).value("dsttfw").toFloat();
    float ajet = qry.record(0).value("ajet").toFloat();
    float rinjt = qry.record(0).value("rinjt").toFloat();
    float routjt = qry.record(0).value("routjt").toFloat();
    float vjt = qry.record(0).value("vjt").toFloat();
    float xjet = qry.record(0).value("xjet").toFloat();
    float yjet = qry.record(0).value("yjet").toFloat();
    float zjet = qry.record(0).value("zjet").toFloat();
    float vxjt = qry.record(0).value("vxjt").toFloat();
    float vyjt = qry.record(0).value("vyjt").toFloat();
    float vzjt = qry.record(0).value("vzjt").toFloat();
    float tempjt = qry.record(0).value("tempjt").toFloat();
    float densjt = qry.record(0).value("densjt").toFloat();
    float anejt = qry.record(0).value("anejt").toFloat();
    float vtrbjt = qry.record(0).value("vtrbjt").toFloat();
    float dstdjt = qry.record(0).value("dstdjt").toFloat();
    float dsttjt = qry.record(0).value("dsttjt").toFloat();
    float aufo = qry.record(0).value("aufo").toFloat();
    float rinuf = qry.record(0).value("rinuf").toFloat();
    float routuf = qry.record(0).value("routuf").toFloat();
    float emuf = qry.record(0).value("emuf").toFloat();
    float ruf = qry.record(0).value("ruf").toFloat();
    float xuf = qry.record(0).value("xuf").toFloat();
    float yuf = qry.record(0).value("yuf").toFloat();
    float zuf = qry.record(0).value("zuf").toFloat();
    float xufo = qry.record(0).value("xufo").toFloat();
    float yufo = qry.record(0).value("yufo").toFloat();
    float zufo = qry.record(0).value("zufo").toFloat();
    float vxuf = qry.record(0).value("vxuf").toFloat();
    float vyuf = qry.record(0).value("vyuf").toFloat();
    float vzuf = qry.record(0).value("vzuf").toFloat();
    float tempuf = qry.record(0).value("tempuf").toFloat();
    float densuf = qry.record(0).value("densuf").toFloat();
    float aneuf = qry.record(0).value("aneuf").toFloat();
    float vtrbuf = qry.record(0).value("vtrbuf").toFloat();
    float edenuf = qry.record(0).value("edenuf").toFloat();
    int ituf = qry.record(0).value("ituf").toInt();
    float etmpuf = qry.record(0).value("etmpuf").toFloat();
    float dstduf = qry.record(0).value("dstduf").toFloat();
    float dsttuf = qry.record(0).value("dsttuf").toFloat();
    float rinsh = qry.record(0).value("rinsh").toFloat();
    float routsh = qry.record(0).value("routsh").toFloat();
    float vsh = qry.record(0).value("vsh").toFloat();
    float evelsh = qry.record(0).value("evelsh").toFloat();
    float rcsh = qry.record(0).value("rcsh").toFloat();
    float vxsh = qry.record(0).value("vxsh").toFloat();
    float vysh = qry.record(0).value("vysh").toFloat();
    float vzsh = qry.record(0).value("vzsh").toFloat();
    float tempsh = qry.record(0).value("tempsh").toFloat();
    float denssh = qry.record(0).value("denssh").toFloat();
    float anesh = qry.record(0).value("anesh").toFloat();
    float vtrbsh = qry.record(0).value("vtrbsh").toFloat();
    float dstdsh = qry.record(0).value("dstdsh").toFloat();
    float dsttsh = qry.record(0).value("dsttsh").toFloat();
    float temp0 = qry.record(0).value("temp0").toFloat();
    float dens0 = qry.record(0).value("dens0").toFloat();
    float ane0 = qry.record(0).value("ane0").toFloat();
    float v0 = qry.record(0).value("v0").toFloat();
    conn.connClose();

    if (file.open(QIODevice::WriteOnly)){
        QTextStream stream(&file);
        stream<<"# alam1[A]  alamn[A]  alams[A]  loglam  cutoff[A]"<<endl;
        stream<<QString().number(alam1, 'f', 2)<< " "<<alamn<< " "<<alams<< " "<<loglam<< " "<<cutoff<<endl;
        stream<<"# imodel  irotat  ipart   ichemc  ielnd"<<endl;
        stream<<imodel<< " "<<irotat<< " "<<ipart<< " "<<ichemc<< " "<<ielnd<<endl;
        stream<<"# ithom   irayl   imie    imiepf  ihyd   iopac  iline   eps     (opacities) "<<endl;
        stream<<ithom<< " "<<irayl<< " "<<imie<< " "<<imiepf<< " "<<ihyd<< " "<<iopac<< " "<<iline<< " "<<eps<<endl;
        stream<<"# ionu  ior  iot  offset "<<endl;
        stream<<ionu<< " "<<ior<< " "<<iot<< " "<<offset<<endl;
        stream<<"# phase1[deg]  phasen[deg]  nphase    dinc[deg] "<<endl;
        stream<<phase1<< " "<<phasen<< " "<<nphase<< " "<<dinc<<endl;
        stream<<"# dd [pc] "<<endl;
        stream<<dd<<endl;
        stream<<"#----------------spectra of the non-transparent objects--------------------- "<<endl;
        stream<<"# lunt1   xunt1   yunt1 "<<endl;
        stream<<lunt1<< " "<<xunt1<< " "<<yunt1<<endl;
        stream<<"# lunt2   xunt2   yunt2 "<<endl;
        stream<<lunt2<< " "<<xunt2<< " "<<yunt2<<endl;
        stream<<"# lunt3   xunt3   yunt3 "<<endl;
        stream<<lunt3<< " "<<xunt3<< " "<<yunt3<<endl;
        stream<<"#------------------------- body frozen grid -------------------------------- "<<endl;
        stream<<"# rmdfx1 rmdfx2 rmdfy1 rmdfy2 rmdfz1 rmdfz2 rmdfx3 rmdfx4 [R_sol] "<<endl;
        stream<<rmdfx1<< " "<<rmdfx2<< " "<<rmdfy1<< " "<<rmdfy2<< " "<<rmdfz1<< " "<<rmdfz2<< " "<<rmdfx3<< " "<<rmdfx4<<endl;
        stream<<"# stepf[R_sol] stepfz[R_sol] gainfx gainfy gainfz "<<endl;
        stream<<stepf<< " "<<stepfz<< " "<<gainfx<< " "<<gainfy<< " "<<gainfz<<endl;
        stream<<"#-------------------------- line of sight grid ----------------------------- "<<endl;
        stream<<"# rmdx1  rmdx2  rmdy1  rmdy2  rmdz1  rmdz2  rmdz3  rmdz4 [R_sol] "<<endl;
        stream<<rmdx1<< " "<<rmdx2<< " "<<rmdy1<< " "<<rmdy2<< " "<<rmdz1<< " "<<rmdz2<< " "<<rmdz3<< " "<<rmdz4<<endl;
        stream<<"# steps[R_sol] stepsz[R_sol] gainx  gainy  gainz "<<endl;
        stream<<steps<< " "<<stepsz<< " "<<gainx<< " "<<gainy<< " "<<gainz<<endl;
        stream<<"#-----------------------------objects--------------------------------------- "<<endl;
        stream<<"# ISTAR# ICOMP# IENV# ISPOT# ISM#  IRING# IDISC# "<<endl;
        stream<<ISTAR<< " "<<ICOMP<< " "<<IENV<< " "<<ISPOT<< " "<<ISM<< " "<<IRING<< " "<<IDISC<<endl;
        stream<<"# INEBL# IFLOW# IJET# IUFO#  ISHELL# "<<endl;
        stream<<INEBL<< " "<<IFLOW<< " "<<IJET<< " "<<IUFO<< " "<<ISHELL<<endl;
        stream<<"#---------------------------- central star --------------------------------- "<<endl;
        stream<<"# rstar[R_sol]   Tstar[K]   eMstar[M_sol] "<<endl;
        stream<<rstar<< " "<<Tstar<< " "<<eMstar<<endl;
        stream<<"# xstar  ystar  zstar  vrotst[km/s] (rotation) "<<endl;
        stream<<xstar<< " "<<ystar<< " "<<zstar<< " "<<vrotst<<endl;
        stream<<"# idifst drotst hst                 (differential rotation) "<<endl;
        stream<<idifst<< " "<<drotst<< " "<<hst<<endl;
        stream<<"# vxst[km/s] vyst[km/s] vzst[km/s]  (movement) "<<endl;
        stream<<vxst<< " "<<vyst<< " "<<vzst<<endl;
        stream<<"# dlst   dlst2  dgst   ffst         (darkening+shape) "<<endl;
        stream<<dlst<< " "<<dlst2<< " "<<dgst<< " "<<ffst<<endl;
        stream<<"# irrst  ialbst albst  htst  htsta  (reflection effect) "<<endl;
        stream<<irrst<< " "<<ialbst<< " "<<albst<< " "<<htst<< " "<<htsta<<endl;
        stream<<"# ispst xspst yspst zspst aspst[deg] tspst   (spot) "<<endl;
        stream<<ispst<< " "<<xspst<< " "<<yspst<< " "<<zspst<< " "<<aspst<< " "<<tspst<<endl;
        stream<<"#----------------------------- companion ----------------------------------- "<<endl;
        stream<<"# rcp[R_sol]  tempcp[K]   qq "<<endl;
        stream<<rcp<< " "<<tempcp<< " "<<qq<<endl;
        stream<<" "<<endl;
        stream<<vrxcp<< " "<<vrycp<< " "<<vrzcp<< " "<<vrotcp<<endl;
        stream<<" "<<endl;
        stream<<xcp<< " "<<ycp<< " "<<zcp<<endl;
        stream<<" "<<endl;
        stream<<vxcp<< " "<<vycp<< " "<<vzcp<<endl;
        stream<<" "<<endl;
        stream<<dlcp<< " "<<dlcp2<< " "<<dgcp<< " "<<ffcp<<endl;
        stream<<" "<<endl;
        stream<<irrcp<< " "<<ialbcp<< " "<<albcp<< " "<<htcp<< " "<<htcpa<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<emen<< " "<<qqen<< " "<<aen<< " "<<ffen<< " "<<hen<<endl;
        stream<<" "<<endl;
        stream<<tempen<< " "<<densen<< " "<<aneen<< " "<<vtrben<< " "<<dstden<< " "<<dstten<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<vrxsp<< " "<<vrysp<< " "<<vrzsp<< " "<<vrotsp<< " "<<rsp<<endl;
        stream<<" "<<endl;
        stream<<xsp<< " "<<ysp<< " "<<zsp<< " "<<vxsp<< " "<<vysp<< " "<<vzsp<<endl;
        stream<<" "<<endl;
        stream<<tempsp<< " "<<denssp<< " "<<anesp<< " "<<vtrbsp<< " "<<dstdsp<< " "<<dsttsp<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<v1sm<< " "<<v2sm<< " "<<r1sm<< " "<<r2sm<<endl;
        stream<<" "<<endl;
        stream<<x1sm<< " "<<y1sm<< " "<<z1sm<<endl;
        stream<<" "<<endl;
        stream<<x2sm<< " "<<y2sm<< " "<<z2sm<<endl;
        stream<<" "<<endl;
        stream<<vxsm<< " "<<vysm<< " "<<vzsm<<endl;
        stream<<" "<<endl;
        stream<<xsm<< " "<<ysm<< " "<<zsm<< " "<<psm<<endl;
        stream<<" "<<endl;
        stream<<tempsm<< " "<<denssm<< " "<<anesm<< " "<<vtrbsm<< " "<<edensm<< " "<<dstdsm<< " "<<dsttsm<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<rrg<< " "<<emrg<<endl;
        stream<<" "<<endl;
        stream<<b1rg<< " "<<b2rg<<endl;
        stream<<" "<<endl;
        stream<<a1rg<< " "<<a2rg<< " "<<dr1rg<< " "<<dr2rg<<endl;
        stream<<" "<<endl;
        stream<<xrg<< " "<<yrg<< " "<<zrg<<endl;
        stream<<" "<<endl;
        stream<<xpolrg<< " "<<ypolrg<< " "<<zpolrg<<endl;
        stream<<" "<<endl;
        stream<<vxrg<< " "<<vyrg<< " "<<vzrg<<endl;
        stream<<" "<<endl;
        stream<<temprg<< " "<<densrg<< " "<<anerg<< " "<<vtrbrg<< " "<<itrg<<endl;
        stream<<" "<<endl;
        stream<<edenrg<< " "<<dstdrg<< " "<<ede2rg<< " "<<dst2rg<< " "<<dsttrg<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<adisc<< " "<<rindc<< " "<<routdc<< " "<<emdc<< " "<<rdc<<endl;
        stream<<" "<<endl;
        stream<<xdc<< " "<<ydc<< " "<<zdc<<endl;
        stream<<" "<<endl;
        stream<<xdisc<< " "<<ydisc<< " "<<zdisc<<endl;
        stream<<" "<<endl;
        stream<<vxdc<< " "<<vydc<< " "<<vzdc<<endl;
        stream<<" "<<endl;
        stream<<tempdc<< " "<<densdc<< " "<<anedc<< " "<<vtrbdc<< " "<<edendc<< " "<<itdc<< " "<<etmpdc<<endl;
        stream<<" "<<endl;
        stream<<dstddc<< " "<<dsttdc<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<aneb<< " "<<rinnb<< " "<<routnb<< " "<<emnb<< " "<<rnb<<endl;
        stream<<" "<<endl;
        stream<<hinvnb<< " "<<tinvnb<< " "<<hwindnb<< " "<<idennb<<endl;
        stream<<" "<<endl;
        stream<<xneb<< " "<<yneb<< " "<<zneb<<endl;
        stream<<" "<<endl;
        stream<<vxnb<< " "<<vynb<< " "<<vznb<<endl;
        stream<<" "<<endl;
        stream<<tempnb<< " "<<densnb<< " "<<anenb<< " "<<vtrbnb<< " "<<edennb<< " "<<itnb<< " "<<etmpnb<<endl;
        stream<<" "<<endl;
        stream<<dstdnb<< " "<<dsttnb<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<v1fw<< " "<<v2fw<< " "<<r1fw<< " "<<r2fw<<endl;
        stream<<" "<<endl;
        stream<<x1fw<< " "<<y1fw<< " "<<z1fw<<endl;
        stream<<" "<<endl;
        stream<<x2fw<< " "<<y2fw<< " "<<z2fw<<endl;
        stream<<" "<<endl;
        stream<<vxfw<< " "<<vyfw<< " "<<vzfw<<endl;
        stream<<" "<<endl;
        stream<<xfw<< " "<<yfw<< " "<<zfw<< " "<<pfw<<endl;
        stream<<" "<<endl;
        stream<<tempfw<< " "<<densfw<< " "<<anefw<< " "<<vtrbfw<< " "<<edenfw<< " "<<dstdfw<< " "<<dsttfw<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<ajet<< " "<<rinjt<< " "<<routjt<< " "<<vjt<<endl;
        stream<<" "<<endl;
        stream<<xjet<< " "<<yjet<< " "<<zjet<<endl;
        stream<<" "<<endl;
        stream<<vxjt<< " "<<vyjt<< " "<<vzjt<<endl;
        stream<<" "<<endl;
        stream<<tempjt<< " "<<densjt<< " "<<anejt<< " "<<vtrbjt<< " "<<dstdjt<< " "<<dsttjt<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<aufo<< " "<<rinuf<< " "<<routuf<< " "<<emuf<< " "<<ruf<<endl;
        stream<<" "<<endl;
        stream<<xuf<< " "<<yuf<< " "<<zuf<<endl;
        stream<<" "<<endl;
        stream<<xufo<< " "<<yufo<< " "<<zufo<<endl;
        stream<<" "<<endl;
        stream<<vxuf<< " "<<vyuf<< " "<<vzuf<<endl;
        stream<<" "<<endl;
        stream<<tempuf<< " "<<densuf<< " "<<aneuf<< " "<<vtrbuf<< " "<<edenuf<< " "<<ituf<< " "<<etmpuf<<endl;
        stream<<" "<<endl;
        stream<<dstduf<< " "<<dsttuf<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<rinsh<< " "<<routsh<< " "<<vsh<<endl;
        stream<<" "<<endl;
        stream<<evelsh<< " "<<rcsh<<endl;
        stream<<" "<<endl;
        stream<<vxsh<< " "<<vysh<< " "<<vzsh<<endl;
        stream<<" "<<endl;
        stream<<tempsh<< " "<<denssh<< " "<<anesh<< " "<<vtrbsh<< " "<<dstdsh<< " "<<dsttsh<<endl;
        stream<<" "<<endl;
        stream<<" "<<endl;
        stream<<temp0<< " "<<dens0<< " "<<ane0<< " "<<v0<<endl;
    }
}

void include_funcs::load_input_file(std::string input_file)
{
    float alam1, alamn, alams, cutoff, offset, phase1, phasen, dinc, dd, xunt1, yunt1, xunt2, yunt2, xunt3, yunt3, rmdfx1, rmdfx2, rmdfy1, rmdfy2, rmdfz1, rmdfz2, rmdfx3, rmdfx4, stepf,  stepfz, gainfx, gainfy, gainfz, rmdx1, rmdx2, rmdy1, rmdy2, rmdz1, rmdz2, rmdz3, rmdz4, steps, stepsz, gainx, gainy, gainz, rstar, Tstar, eMstar, xstar, ystar, zstar, vrotst, drotst, hst, vxst, vyst, vzst, dlst, dlst2, dgst, ffst, albst, htst, htsta, xspst, yspst, zspst, aspst, tspst;
    float rcp, tempcp, qq, vrxcp, vrycp, vrzcp, vrotcp, xcp, ycp, zcp, vxcp, vycp, vzcp, dlcp, dlcp2, dgcp, ffcp, albcp, htcp, htcpa, emen, qqen, aen, ffen, hen, tempen, densen, aneen, vtrben, dstden, dstten, vrxsp, vrysp, vrzsp, vrotsp, rsp, xsp, ysp, zsp, vxsp, vysp, vzsp, tempsp, denssp, anesp, vtrbsp, dstdsp, dsttsp;
    double eps;
    int loglam, imodel, irotat, ipart, ichemc, ielnd, ithom, irayl, imie, imiepf, ihyd, iopac, iline, ionu, ior, iot, nphase, lunt1, lunt2, lunt3, istar, icomp, ienv, ispot, ism, iring, idisc, inebl, iflow, ijet, iufo, ishell,  idifst, irrst, ialbst, ispst, irrcp, ialbcp;

    qDebug() << "Getting to load_input_file function." << endl;

    std::ifstream f(input_file);
    std::string line;

    std::getline(f, line);
    std::getline(f, line);
    std::stringstream ss(line);
    ss >> alam1 >> alamn >> alams >> loglam >> cutoff;
//    std::cout << alam1 << alamn << alams << loglam << cutoff << std::endl;

    std::getline(f, line);
//    std::cout << line;
    std::getline(f, line);
//    std::cout << line << std::endl;
    ss.str(line);
    ss.clear();
    ss >> imodel >> irotat >> ipart >> ichemc >> ielnd;
//    std::cout << imodel << irotat << ipart << ichemc << ielnd << std::endl;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> ithom >> irayl >> imie >> imiepf >> ihyd >> iopac >> iline >> eps;
//    std::cout << ithom << irayl << imie << eps << std::endl;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> ionu >> ior >> iot >> offset;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> phase1 >> phasen >> nphase >> dinc;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> dd;

    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> lunt1 >> xunt1 >> yunt1;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> lunt2 >> xunt2 >> yunt2;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> lunt3 >> xunt3 >> yunt3;

    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> rmdfx1 >> rmdfx2 >> rmdfy1 >> rmdfy2 >> rmdfz1 >> rmdfz2 >> rmdfx3 >> rmdfx4;
//    std::cout << rmdfx1 << " " << rmdfz2 << std::endl;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> stepf >>  stepfz >> gainfx >> gainfy >> gainfz;

    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> rmdx1 >> rmdx2 >> rmdy1 >> rmdy2 >> rmdz1 >> rmdz2 >> rmdz3 >> rmdz4;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> steps >> stepsz >> gainx >> gainy >> gainz;
//    std::cout << steps << " " << stepsz << " " << gainz << std::endl;

    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> istar >> icomp >> ienv >> ispot >> ism >> iring >> idisc;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> inebl >> iflow >> ijet >> iufo >> ishell;

    // central star
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    std::cout << line << std::endl;
    ss.str(line);
    ss.clear();
    ss >> rstar >> Tstar >> eMstar;
    std::cout << rstar << " " << Tstar << " " << eMstar << std::endl;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xstar >> ystar >> zstar >> vrotst;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> idifst >> drotst >> hst;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxst >> vyst >> vzst;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> dlst >> dlst2 >> dgst >> ffst;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> irrst >> ialbst >> albst >> htst >> htsta;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> ispst >> xspst >> yspst >> zspst >> aspst >> tspst;

    // companion
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> rcp >> tempcp >> qq;
    qDebug() << rcp << tempcp << qq;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vrxcp >> vrycp >> vrzcp >> vrotcp;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xcp >> ycp >> zcp;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxcp >> vycp >> vzcp;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> dlcp >> dlcp2 >> dgcp >> ffcp;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> irrcp >> ialbcp >> albcp >> htcp >> htcpa;

    // envelope
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> emen >> qqen >> aen >> ffen >> hen;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempen >> densen >> aneen >> vtrben >> dstden >> dstten;

    // spot
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vrxsp >> vrysp >> vrzsp >> vrotsp >> rsp;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xsp >> ysp >> zsp >> vxsp >> vysp >> vzsp;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempsp >> denssp >> anesp >> vtrbsp >> dstdsp >> dsttsp;

    // stream
    float v1sm, v2sm, r1sm, r2sm, x1sm, y1sm, z1sm, x2sm, y2sm, z2sm, vxsm, vysm, vzsm, xsm, ysm, zsm, Psm, tempsm, denssm, anesm, vtrbsm, edensm, dstdsm, dsttsm;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> v1sm >> v2sm >> r1sm >> r2sm;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> x1sm >> y1sm >> z1sm;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> x2sm >> y2sm >> z2sm;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxsm >> vysm >> vzsm;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xsm >> ysm >> zsm >> Psm;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempsm >> denssm >> anesm >> vtrbsm >> edensm >> dstdsm >> dsttsm;

    // ring
    float rrg, emrg, b1rg, b2rg, a1rg, a2rg, dr1rg, dr2rg, xrg, yrg, zrg, xpolrg, ypolrg, zpolrg, vxrg, vyrg, vzrg, temprg, densrg, anerg, vtrbrg, edenrg, dstdrg, ede2rg, dst2rg, dsttrg;
    int itrg;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> rrg >> emrg;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> b1rg >> b2rg;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> a1rg >> a2rg >> dr1rg >> dr2rg;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xrg >> yrg >> zrg;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xpolrg >> ypolrg >> zpolrg;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxrg >> vyrg >> vzrg;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> temprg >> densrg >> anerg >> vtrbrg >> itrg;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> edenrg >> dstdrg >> ede2rg >> dst2rg >> dsttrg;

    // disc
    float adisc, rindc, routdc, emdc, rdc, xdc, ydc, zdc, xdisc, ydisc, zdisc, vxdc, vydc, vzdc, tempdc, densdc, anedc, vtrbdc, edendc, etmpdc, dstddc, dsttdc;
    int itdc;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> adisc >> rindc >> routdc >> emdc >> rdc;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xdc >> ydc >> zdc;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xdisc >> ydisc >> zdisc;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxdc >> vydc >> vzdc;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempdc >> densdc >> anedc >> vtrbdc >> edendc >> itdc >> etmpdc;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> dstddc >> dsttdc;

    // nebula
    float aneb, rinnb, routnb, emnb, rnb, xneb, yneb, zneb, vxnb, vynb, vznb, tempnb, densnb, anenb, vtrbnb, edennb, etmpnb, dstdnb, dsttnb;
    double hinvnb, tinvnb, hwindnb;
    int idennb, itnb;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> aneb >> rinnb >> routnb >> emnb >> rnb;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> hinvnb >> tinvnb >> hwindnb >> idennb;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xneb >> yneb >> zneb;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxnb >> vynb >> vznb;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempnb >> densnb >> anenb >> vtrbnb >> edennb >> itnb >> etmpnb;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> dstdnb >> dsttnb;

    // flow
    float v1fw, v2fw, r1fw, r2fw, x1fw, y1fw, z1fw, x2fw, y2fw, z2fw, vxfw, vyfw, vzfw, xfw, yfw, zfw, Pfw, tempfw, densfw, anefw, vtrbfw, edenfw, dstdfw, dsttfw;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> v1fw >> v2fw >> r1fw >> r2fw;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> x1fw >> y1fw >> z1fw;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> x2fw >> y2fw >> z2fw;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxfw >> vyfw >> vzfw;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xfw >> yfw >> zfw >> Pfw;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempfw >> densfw >> anefw >> vtrbfw >> edenfw >> dstdfw >> dsttfw;

    // jet
    float ajet, rinjt, routjt, vjt, xjet, yjet, zjet, vxjt, vyjt, vzjt, tempjt, densjt, anejt, vtrbjt, dstdjt, dsttjt;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> ajet >> rinjt >> routjt >> vjt;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xjet >> yjet >> zjet;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxjt >> vyjt >> vzjt;
//    qDebug() << vxjt << vyjt << vzjt;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempjt >> densjt >> anejt >> vtrbjt >> dstdjt >> dsttjt;

    // ufo
    float aufo, rinuf, routuf, emuf, ruf, xuf, yuf, zuf, xufo, yufo, zufo, vxuf, vyuf, vzuf, tempuf, densuf, aneuf, vtrbuf, edenuf, etmuf, dstduf, dsttuf;
    int ituf;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> aufo >> rinuf >> routuf >> emuf >> ruf;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xuf >> yuf >> zuf;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> xufo >> yufo >> zufo;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxuf >> vyuf >> vzuf;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempuf >> densuf >> aneuf >> vtrbuf >> edenuf >> ituf >> etmuf;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> dstduf >> dsttuf;
//    qDebug() << dstduf << dsttuf;

    // shell
    float rinsh, routsh, vsh, evelsh, rcsh, vxsh, vysh, vzsh, tempsh, denssh, anesh, vtrbsh, dstdsh, dsttsh;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> rinsh >> routsh >> vsh;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> evelsh >> rcsh;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> vxsh >> vysh >> vzsh;

    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> tempsh >> denssh >> anesh >> vtrbsh >> dstdsh >> dsttsh;

    // background
    float temp0, dens0, ane0, v0;
    std::getline(f, line);
    std::getline(f, line);
    std::getline(f, line);
    ss.str(line);
    ss.clear();
    ss >> temp0 >> dens0 >> ane0 >> v0;
//    qDebug() << temp0 << dens0 << ane0 << v0 << endl;

    // Saving inputs in the DB
    shellspec shmain;
    shmain.connOpen();
    QSqlQuery qry;
    qry.prepare("UPDATE user_inputs SET alam1=:alam1, alamn=:alamn, alams=:alams, loglam=:loglam,"
                "  cutoff=:cutoff, imodel=:imodel, imodel=:imodel, ipart=:ipart, ichemc=:ichemc, ielnd=:ielnd, ithom=:ithom, irayl=:irayl, imie=:imie, imiepf=:imiepf, ihyd=:ihyd, iopac=:iopac, iline=:iline, eps=:eps, ionu=:ionu, ior=:ior, iot=:iot, offset=:offset, phase1=:phase1, phasen=:phasen,"
                " nphase=:nphase, dinc=:dinc, dd=:dd, lunt1=:lunt1, xunt1=:xunt1, yunt1=:yunt1, lunt2=:lunt2, xunt2=:xunt2, yunt2=:yunt2, lunt3=:lunt3, xunt3=:xunt3, yunt3=:yunt3, rmdfx1=:rmdfx1, rmdfx2=:rmdfx2, rmdfy1=:rmdfy1, rmdfy2=:rmdfy2, rmdfz1=:rmdfz1, rmdfz2=:rmdfz2, rmdfx3=:rmdfx3, rmdfx4=:rmdfx4,"
                " stepf=:stepf, stepfz=:stepfz, gainfx=:gainfx, gainfy=:gainfy, gainfz=:gainfz, rmdx1=:rmdx1, rmdx2=:rmdx2, rmdy1=:rmdy1, rmdy2=:rmdy2, rmdz1=:rmdz1, rmdz2=:rmdz2, rmdz3=:rmdz3, rmdz4=:rmdz4, steps=:steps, stepsz=:stepsz, gainx=:gainx, gainy=:gainy, gainz=:gainz,"
                "ISTAR=:istar, rstar=:rstar, Tstar=:tstar, eMstar=:emstar,"
                " ichemc=:ichemc, ielnd=:ielnd,"
                " xstar=:xstar, ystar=:ystar, zstar=:zstar, vrotst=:vrotst, idifst=:idifst,"
                " drotst=:drotst, hst=:hst, vxst=:vxst, vyst=:vyst, vzst=:vzst, dlst=:dlst,"
                " dlst2=:dlst2, dgst=:dgst, ffst=:ffst, irrst=:irrst, ialbst=:ialbst, albst=:albst,"
                " htst=:htst, htsta=:htsta, ispst=:ispst, xspst=:xspst, yspst=:yspst,"
                " zspst=:zspst, aspst=:aspst, tspst=:tspst,"
                "ICOMP=:icomp, rcp=:rcp, tempcp=:tempcp,"
                " qq=:qq, vrxcp=:vrxcp, vrycp=:vrycp, vrzcp=:vrzcp, vrotcp=:vrotcp, xcp=:xcp,"
                " ycp=:ycp, zcp=:zcp, vxcp=:vxcp, vycp=:vycp, vzcp=:vzcp, dlcp=:dlcp, dlcp2=:dlcp2,"
                " dgcp=:dgcp, ffcp=:ffcp, irrcp=:irrcp, ialbcp=:ialbcp, albcp=:albcp, htcp=:htcp,"
                " htcpa=:htcpa,"
                " IENV=:ienv, emen=:emen, qqen=:qqen, aen=:aen, ffen=:ffen,"
                " hen=:hen, tempen=:tempen, densen=:densen, aneen=:aneen, vtrben=:vtrben,"
                " dstden=:dstden, dstten=:dstten,"
                "ISPOT=:ispot, vrxsp=:vrxsp, vrysp=:vrysp,"
                " vrzsp=:vrzsp, vrotsp=:vrotsp, rsp=:rsp, xsp=:xsp, ysp=:ysp, zsp=:zsp,"
                " vxsp=:vxsp, vysp=:vysp, vzsp=:vzsp, tempsp=:tempsp, denssp=:denssp,"
                " anesp=:anesp, vtrbsp=:vtrbsp, dstdsp=:dstdsp, dsttsp=:dsttsp,"
                " ISM=:ism, v1sm=:v1sm, v2sm=:v2sm, r1sm=:r1sm, r2sm=:r2sm, x1sm=:x1sm,"
                " y1sm=:y1sm, z1sm=:z1sm, x2sm=:x2sm, y2sm=:y2sm, z2sm=:z2sm, vxsm=:vxsm,"
                " vysm=:vysm, vzsm=:vzsm, xsm=:xsm, ysm=:ysm, zsm=:zsm, psm=:psm,"
                " tempsm=:tempsm, denssm=:denssm,anesm=:anesm, vtrbsm=:vtrbsm,"
                " edensm=:edensm, dstdsm=:dstdsm, dsttsm=:dsttsm,"
                "IRING=:iring, rrg=:rrg, emrg=:emrg, b1rg=:b1rg, b2rg=:b2rg, a1rg=:a1rg, a2rg=:a2rg,"
                " dr1rg=:dr1rg, dr2rg=:dr2rg, xrg=:xrg, yrg=:yrg, zrg=:zrg, xpolrg=:xpolrg, ypolrg=:ypolrg,"
                " zpolrg=:zpolrg, vxrg=:vxrg, vyrg=:vyrg, vzrg=:vzrg, temprg=:temprg, densrg=:densrg,"
                " anerg=:anerg, vtrbrg=:vtrbrg, itrg=:itrg, edenrg=:edenrg, dstdrg=:dstdrg,"
                " ede2rg=:ede2rg, dst2rg=:dst2rg, dsttrg=:dsttrg,"
                "IDISC=:idisc, adisc=:adisc, rindc=:rindc, routdc=:routdc, emdc=:emdc,"
                " rdc=:rdc, xdc=:xdc, ydc=:ydc, zdc=:zdc, xdisc=:xdisc, ydisc=:ydisc,"
                " zdisc=:zdisc, vxdc=:vxdc, vydc=:vydc, vzdc=:vzdc, tempdc=:tempdc,"
                " densdc=:densdc, anedc=:anedc, vtrbdc=:vtrbdc, edendc=:edendc, itdc=:itdc, dstddc=:dstddc,"
                " dsttdc=:dsttdc,"
                "INEBL=:inebl, aneb=:aneb, rinnb=:rinnb, routnb=:routnb, emnb=:emnb, rnb=:rnb,"
                " hinvnb=:hinvnb, tinvnb=:tinvnb, hwindnb=:hwindnb, idennb=:idennb, xneb=:xneb,"
                " yneb=:yneb, zneb=:zneb, vxnb=:vxnb, vynb=:vynb,"
                " vznb=:vznb, tempnb=:tempnb, densnb=:densnb, anenb=:anenb, vtrbnb=:vtrbnb,"
                " edennb=:edennb, itnb=:itnb, etmpnb=:etmpnb, dstdnb=:dstdnb, dsttnb=:dsttnb,"
                "IFLOW=:iflow, v1fw=:v1fw, v2fw=:v2fw, r1fw=:r1fw, r2fw=:r2fw, x1fw=:x1fw,"
                " y1fw=:y1fw, z1fw=:z1fw, x2fw=:x2fw, y2fw=:y2fw, z2fw=:z2fw, vxfw=:vxfw,"
                " vyfw=:vyfw, vzfw=:vzfw, xfw=:xfw, yfw=:yfw, zfw=:zfw, pfw=:pfw, tempfw=:tempfw,"
                " densfw=:densfw, anefw=:anefw, vtrbfw=:vtrbfw, edenfw=:edenfw, dstdfw=:dstdfw,"
                " dsttfw=:dsttfw,"
                "IJET=:ijet, ajet=:ajet, rinjt=:rinjt, routjt=:routjt, vjt=:vjt, xjet=:xjet,"
                " yjet=:yjet, zjet=:zjet, vxjt=:vxjt, vyjt=:vyjt, vzjt=:vzjt, tempjt=:tempjt,"
                " densjt=:densjt, anejt=:anejt, vtrbjt=:vtrbjt, dstdjt=:dstdjt, dsttjt=:dsttjt,"
                "IUFO=:iufo, aufo=:aufo, rinuf=:rinuf, routuf=:routuf, emuf=:emuf, ruf=:ruf,"
                " xuf=:xuf, yuf=:yuf, zuf=:zuf, xufo=:xufo, yufo=:yufo, zufo=:zufo,"
                " vxuf=:vxuf, vyuf=:vyuf, vzuf=:vzuf, tempuf=:tempuf, densuf=:densuf,"
                " aneuf=:aneuf, vtrbuf=:vtrbuf, edenuf=:edenuf, ituf=:ituf, etmpuf=:etmpuf,"
                " dstduf=:dstduf, dsttuf=:dsttuf,"
                "ISHELL=:ishell, rinsh=:rinsh, routsh=:routsh, vsh=:vsh, evelsh=:evelsh,"
                " rcsh=:rcsh, vxsh=:vxsh, vysh=:vysh, vzsh=:vzsh, tempsh=:tempsh, denssh=:denssh,"
                " anesh=:anesh, vtrbsh=:vtrbsh, dstdsh=:dstdsh, dsttsh=:dsttsh"
                " WHERE ID=2");
    qry.bindValue(":alam1", alam1);
    qry.bindValue(":alamn", alamn);
    qry.bindValue(":alams", alams);
    qry.bindValue(":loglam", loglam);
    qry.bindValue(":cutoff", cutoff);
    qry.bindValue(":imodel", imodel);
    qry.bindValue(":irotat", irotat);
    qry.bindValue(":ipart", ipart);
    qry.bindValue(":ichemc", ichemc);
    qry.bindValue(":ielnd", ielnd);
    qry.bindValue(":ithom", ithom);
    qry.bindValue(":irayl", irayl);
    qry.bindValue(":imie", imie);
    qry.bindValue(":imiepf", imiepf);
    qry.bindValue(":ihyd", ihyd);
    qry.bindValue(":iopac", iopac);
    qry.bindValue(":iline", iline);
    qry.bindValue(":eps", eps);
    qry.bindValue(":ionu", ionu);
    qry.bindValue(":ior", ior);
    qry.bindValue(":iot", iot);
    qry.bindValue(":offset", offset);
    qry.bindValue(":phase1", phase1);
    qry.bindValue(":phasen", phasen);
    qry.bindValue(":nphase", nphase);
    qry.bindValue(":dinc", dinc);
    qry.bindValue(":dd", dd);
    qry.bindValue(":lunt1", lunt1);
    qry.bindValue(":xunt1", xunt1);
    qry.bindValue(":yunt1", yunt1);
    qry.bindValue(":lunt2", lunt2);
    qry.bindValue(":xunt2", xunt2);
    qry.bindValue(":yunt2", yunt2);
    qry.bindValue(":lunt3", lunt3);
    qry.bindValue(":xunt3", xunt3);
    qry.bindValue(":yunt3", yunt3);
    qry.bindValue(":rmdfx1", rmdfx1);
    qry.bindValue(":rmdfx2", rmdfx2);
    qry.bindValue(":rmdfy1", rmdfy1);
    qry.bindValue(":rmdfy2", rmdfy2);
    qry.bindValue(":rmdfz1", rmdfz1);
    qry.bindValue(":rmdfz2", rmdfz2);
    qry.bindValue(":rmdfx3", rmdfx3);
    qry.bindValue(":rmdfx4", rmdfx4);
    qry.bindValue(":stepf", stepf);
    qry.bindValue(":stepfz", stepfz);
    qry.bindValue(":gainfx", gainfx);
    qry.bindValue(":gainfy", gainfy);
    qry.bindValue(":gainfz", gainfz);
    qry.bindValue(":rmdx1", rmdx1);
    qry.bindValue(":rmdx2", rmdx2);
    qry.bindValue(":rmdy1", rmdy1);
    qry.bindValue(":rmdy2", rmdy2);
    qry.bindValue(":rmdz1", rmdz1);
    qry.bindValue(":rmdz2", rmdz2);
    qry.bindValue(":rmdz3", rmdz3);
    qry.bindValue(":rmdz4", rmdz4);
    qry.bindValue(":steps", steps);
    qry.bindValue(":stepsz", stepsz);
    qry.bindValue(":gainx", gainx);
    qry.bindValue(":gainy", gainy);
    qry.bindValue(":gainz", gainz);
    qry.bindValue(":ichemc", ichemc);
    qry.bindValue(":ielnd", ielnd);
    qry.bindValue(":istar", istar);
    qry.bindValue(":rstar", rstar);
    qry.bindValue(":tstar", Tstar);
    qry.bindValue(":emstar", eMstar);
    qry.bindValue(":xstar", xstar);
    qry.bindValue(":ystar", ystar);
    qry.bindValue(":zstar", zstar);
    qry.bindValue(":vrotst", vrotst);
    qry.bindValue(":idifst", idifst);
    qry.bindValue(":drotst", drotst);
    qry.bindValue(":hst", hst);
    qry.bindValue(":vxst", vxst);
    qry.bindValue(":vyst", vyst);
    qry.bindValue(":vzst", vzst);
    qry.bindValue(":dlst", dlst);
    qry.bindValue(":dlst2", dlst2);
    qry.bindValue(":dgst", dgst);
    qry.bindValue(":ffst", ffst);
    qry.bindValue(":irrst", irrst);
    qry.bindValue(":ialbst", ialbst);
    qry.bindValue(":albst", albst);
    qry.bindValue(":htst", htst);
    qry.bindValue(":htsta", htsta);
    qry.bindValue(":ispst", ispst);
    qry.bindValue(":xspst", xspst);
    qry.bindValue(":yspst", yspst);
    qry.bindValue(":zspst", zspst);
    qry.bindValue(":aspst", aspst);
    qry.bindValue(":tspst", tspst);
    qry.bindValue(":icomp", icomp);
    qry.bindValue(":rcp", rcp);
    qry.bindValue(":tempcp", tempcp);
    qry.bindValue(":qq", qq);
    qry.bindValue(":vrxcp", vrxcp);
    qry.bindValue(":vrycp", vrycp);
    qry.bindValue(":vrzcp", vrzcp);
    qry.bindValue(":vrotcp", vrotcp);
    qry.bindValue(":xcp", xcp);
    qry.bindValue(":ycp", ycp);
    qry.bindValue(":zcp", zcp);
    qry.bindValue(":vxcp", vxcp);
    qry.bindValue(":vycp", vycp);
    qry.bindValue(":vzcp", vzcp);
    qry.bindValue(":dlcp", dlcp);
    qry.bindValue(":dlcp2", dlcp2);
    qry.bindValue(":dgcp", dgcp);
    qry.bindValue(":ffcp", ffcp);
    qry.bindValue(":irrcp", irrcp);
    qry.bindValue(":ialbcp", ialbcp);
    qry.bindValue(":albcp", albcp);
    qry.bindValue(":htcp", htcp);
    qry.bindValue(":htcpa", htcpa);
    qry.bindValue(":ienv", ienv);
    qry.bindValue(":emen", emen);
    qry.bindValue(":qqen", qqen);
    qry.bindValue(":aen", aen);
    qry.bindValue(":ffen", ffen);
    qry.bindValue(":hen", hen);
    qry.bindValue(":tempen", tempen);
    qry.bindValue(":densen", densen);
    qry.bindValue(":aneen", aneen);
    qry.bindValue(":vtrben", vtrben);
    qry.bindValue(":dstden", dstden);
    qry.bindValue(":dstten", dstten);
    qry.bindValue(":ispot", ispot);
    qry.bindValue(":vrxsp", vrxsp);
    qry.bindValue(":vrysp", vrysp);
    qry.bindValue(":vrzsp", vrzsp);
    qry.bindValue(":vrotsp", vrotsp);
    qry.bindValue(":rsp", rsp);
    qry.bindValue(":xsp", xsp);
    qry.bindValue(":ysp", ysp);
    qry.bindValue(":zsp", zsp);
    qry.bindValue(":vxsp", vxsp);
    qry.bindValue(":vysp", vysp);
    qry.bindValue(":vzsp", vzsp);
    qry.bindValue(":tempsp", tempsp);
    qry.bindValue(":denssp", denssp);
    qry.bindValue(":anesp", anesp);
    qry.bindValue(":vtrbsp", vtrbsp);
    qry.bindValue(":dstdsp", dstdsp);
    qry.bindValue(":dsttsp", dsttsp);
    qry.bindValue(":ism", ism);
    qry.bindValue(":v1sm", v1sm);
    qry.bindValue(":v2sm", v2sm);
    qry.bindValue(":r1sm", r1sm);
    qry.bindValue(":r2sm", r2sm);
    qry.bindValue(":x1sm", x1sm);
    qry.bindValue(":y1sm", y1sm);
    qry.bindValue(":z1sm", z1sm);
    qry.bindValue(":x2sm", x2sm);
    qry.bindValue(":y2sm", y2sm);
    qry.bindValue(":z2sm", z2sm);
    qry.bindValue(":vxsm", vxsm);
    qry.bindValue(":vysm", vysm);
    qry.bindValue(":vzsm", vzsm);
    qry.bindValue(":xsm", xsm);
    qry.bindValue(":ysm", ysm);
    qry.bindValue(":zsm", zsm);
    qry.bindValue(":psm", Psm);
    qry.bindValue(":tempsm", tempsm);
    qry.bindValue(":denssm", denssm);
    qry.bindValue(":anesm", anesm);
    qry.bindValue(":vtrbsm", vtrbsm);
    qry.bindValue(":edensm", edensm);
    qry.bindValue(":dstdsm", dstdsm);
    qry.bindValue(":dsttsm", dsttsm);
    qry.bindValue(":iring", iring);
    qry.bindValue(":rrg", rrg);
    qry.bindValue(":emrg", emrg);
    qry.bindValue(":b1rg", b1rg);
    qry.bindValue(":b2rg", b2rg);
    qry.bindValue(":a1rg", a1rg);
    qry.bindValue(":a2rg", a2rg);
    qry.bindValue(":dr1rg", dr1rg);
    qry.bindValue(":dr2rg", dr2rg);
    qry.bindValue(":xrg", xrg);
    qry.bindValue(":yrg", yrg);
    qry.bindValue(":zrg", zrg);
    qry.bindValue(":xpolrg", xpolrg);
    qry.bindValue(":ypolrg", ypolrg);
    qry.bindValue(":zpolrg", zpolrg);
    qry.bindValue(":vxrg", vxrg);
    qry.bindValue(":vyrg", vyrg);
    qry.bindValue(":vzrg", vzrg);
    qry.bindValue(":temprg", temprg);
    qry.bindValue(":densrg", densrg);
    qry.bindValue(":anerg", anerg);
    qry.bindValue(":vtrbrg", vtrbrg);
    qry.bindValue(":itrg", itrg);
    qry.bindValue(":edenrg", edenrg);
    qry.bindValue(":dstdrg", dstdrg);
    qry.bindValue(":ede2rg", ede2rg);
    qry.bindValue(":dst2rg", dst2rg);
    qry.bindValue(":dsttrg", dsttrg);
    qry.bindValue(":idisc", idisc);
    qry.bindValue(":adisc", adisc);
    qry.bindValue(":rindc", rindc);
    qry.bindValue(":routdc", routdc);
    qry.bindValue(":emdc", emdc);
    qry.bindValue(":rdc", rdc);
    qry.bindValue(":xdc", xdc);
    qry.bindValue(":ydc", ydc);
    qry.bindValue(":zdc", zdc);
    qry.bindValue(":xdisc", xdisc);
    qry.bindValue(":ydisc", ydisc);
    qry.bindValue(":zdisc", zdisc);
    qry.bindValue(":vxdc", vxdc);
    qry.bindValue(":vydc", vydc);
    qry.bindValue(":vzdc", vzdc);
    qry.bindValue(":tempdc", tempdc);
    qry.bindValue(":densdc", densdc);
    qry.bindValue(":anedc", anedc);
    qry.bindValue(":vtrbdc", vtrbdc);
    qry.bindValue(":edendc", edendc);
    qry.bindValue(":itdc", itdc);
    qry.bindValue(":etmpdc", etmpdc);
    qry.bindValue(":dstddc", dstddc);
    qry.bindValue(":dsttdc", dsttdc);
    qry.bindValue(":inebl", inebl);
    qry.bindValue(":aneb", aneb);
    qry.bindValue(":rinnb", rinnb);
    qry.bindValue(":routnb", routnb);
    qry.bindValue(":emnb", emnb);
    qry.bindValue(":rnb", rnb);
    qry.bindValue(":hinvnb", hinvnb);
    qry.bindValue(":tinvnb", tinvnb);
    qry.bindValue(":hwindnb", hwindnb);
    qry.bindValue(":idennb", idennb);
    qry.bindValue(":xneb", xneb);
    qry.bindValue(":yneb", yneb);
    qry.bindValue(":zneb", zneb);
    qry.bindValue(":vxnb", vxnb);
    qry.bindValue(":vynb", vynb);
    qry.bindValue(":vznb", vznb);
    qry.bindValue(":tempnb", tempnb);
    qry.bindValue(":densnb", densnb);
    qry.bindValue(":anenb", anenb);
    qry.bindValue(":vtrbnb", vtrbnb);
    qry.bindValue(":edennb", edennb);
    qry.bindValue(":itnb", itnb);
    qry.bindValue(":etmpnb", etmpnb);
    qry.bindValue(":dstdnb", dstdnb);
    qry.bindValue(":dsttnb", dsttnb);
    qry.bindValue(":iflow", iflow);
    qry.bindValue(":v1fw", v1fw);
    qry.bindValue(":v2fw", v2fw);
    qry.bindValue(":r1fw", r1fw);
    qry.bindValue(":r2fw", r2fw);
    qry.bindValue(":x1fw", x1fw);
    qry.bindValue(":y1fw", y1fw);
    qry.bindValue(":z1fw", z1fw);
    qry.bindValue(":x2fw", x2fw);
    qry.bindValue(":y2fw", y2fw);
    qry.bindValue(":z2fw", z2fw);
    qry.bindValue(":vxfw", vxfw);
    qry.bindValue(":vyfw", vyfw);
    qry.bindValue(":vzfw", vzfw);
    qry.bindValue(":xfw", xfw);
    qry.bindValue(":yfw", yfw);
    qry.bindValue(":zfw", zfw);
    qry.bindValue(":pfw", Pfw);
    qry.bindValue(":tempfw", tempfw);
    qry.bindValue(":densfw", densfw);
    qry.bindValue(":anefw", anefw);
    qry.bindValue(":vtrbfw", vtrbfw);
    qry.bindValue(":edenfw", edenfw);
    qry.bindValue(":dstdfw", dstdfw);
    qry.bindValue(":dsttfw", dsttfw);
    qry.bindValue(":ijet", ijet);
    qry.bindValue(":ajet", ajet);
    qry.bindValue(":rinjt", rinjt);
    qry.bindValue(":routjt", routjt);
    qry.bindValue(":vjt", vjt);
    qry.bindValue(":xjet", xjet);
    qry.bindValue(":yjet", yjet);
    qry.bindValue(":zjet", zjet);
    qry.bindValue(":vxjt", vxjt);
    qry.bindValue(":vyjt", vyjt);
    qry.bindValue(":vzjt", vzjt);
    qry.bindValue(":tempjt", tempjt);
    qry.bindValue(":densjt", densjt);
    qry.bindValue(":anejt", anejt);
    qry.bindValue(":vtrbjt", vtrbjt);
    qry.bindValue(":dstdjt", dstdjt);
    qry.bindValue(":dsttjt", dsttjt);
    qry.bindValue(":iufo", iufo);
    qry.bindValue(":aufo", aufo);
    qry.bindValue(":rinuf", rinuf);
    qry.bindValue(":routuf", routuf);
    qry.bindValue(":emuf", emuf);
    qry.bindValue(":ruf", ruf);
    qry.bindValue(":xuf", xuf);
    qry.bindValue(":yuf", yuf);
    qry.bindValue(":zuf", zuf);
    qry.bindValue(":xufo", xufo);
    qry.bindValue(":yufo", yufo);
    qry.bindValue(":zufo", zufo);
    qry.bindValue(":vxuf", vxuf);
    qry.bindValue(":vyuf", vyuf);
    qry.bindValue(":vzuf", vzuf);
    qry.bindValue(":tempuf", tempuf);
    qry.bindValue(":densuf", densuf);
    qry.bindValue(":aneuf", aneuf);
    qry.bindValue(":vtrbuf", vtrbuf);
    qry.bindValue(":edenuf", edenuf);
    qry.bindValue(":ituf", ituf);
    qry.bindValue(":etmpuf", etmuf);
    qry.bindValue(":dstduf", dstduf);
    qry.bindValue(":dsttuf", dsttuf);
    qry.bindValue(":ishell", ishell);
    qry.bindValue(":rinsh", rinsh);
    qry.bindValue(":routsh", routsh);
    qry.bindValue(":vsh", vsh);
    qry.bindValue(":evelsh", evelsh);
    qry.bindValue(":rcsh", rcsh);
    qry.bindValue(":vxsh", vxsh);
    qry.bindValue(":vysh", vysh);
    qry.bindValue(":vzsh", vzsh);
    qry.bindValue(":tempsh", tempsh);
    qry.bindValue(":denssh", denssh);
    qry.bindValue(":anesh", anesh);
    qry.bindValue(":vtrbsh", vtrbsh);
    qry.bindValue(":dstdsh", dstdsh);
    qry.bindValue(":dsttsh", dsttsh);

    if (qry.exec())
    {
        qDebug()<<"ShellSpec DB correctly."<<endl;
        shmain.connClose();
    }
    else {
        qDebug()<<"Error ShellSpec DB."<<qry.lastError()<<endl;
        qDebug()<<qry.lastQuery()<<endl;
        shmain.connClose();
    }
}
