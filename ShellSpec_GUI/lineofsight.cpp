#include "lineofsight.h"
#include "ui_lineofsight.h"
#include <QFileDialog>
#include <QMessageBox>
#include <QFile>

lineOfSight::lineOfSight(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::lineOfSight)
{
    ui->setupUi(this);

    QString field="user";
    set_default_user_inputs(field);
}

lineOfSight::~lineOfSight()
{
    delete ui;
}

void lineOfSight::on_buttonBox_accepted()
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    int ionu = ui->textionu->text().toInt();
    int ior = ui->textiot->text().toInt();
    int iot = ui->textiot->text().toInt();
    QString offset = ui->textOffset->text();
    QString phase1 = ui->textStartPhase->text();
    QString phasen = ui->textEndPhase->text();
    QString nphase = ui->textNoRotation->text();
    QString dinc = ui->textIncAngle->text();
    QString dist = ui->textDistance->text();
    QString rmdx1 = ui->textx1->text();
    QString rmdx2 = ui->textx2->text();
    QString rmdy1 = ui->texty1->text();
    QString rmdy2 = ui->texty2->text();
    QString rmdz1 = ui->textz1->text();
    QString rmdz2 = ui->textz2->text();
    QString rmdz3 = ui->textz3->text();
    QString rmdz4 = ui->textz4->text();
    QString steps = ui->textSteps->text();
    QString stepsz = ui->textStepsz->text();
    QString gainx = ui->textGainx->text();
    QString gainy = ui->textGainy->text();
    QString gainz = ui->textGainz->text();

    if (nphase.toInt() == 0 ){
        QString dir_to_phases = ui->textAddPhases->text();
        QFileInfo check_file(dir_to_phases);
        if (check_file.exists() && check_file.isFile()){
            qDebug()<<"Copied phases!";
            QFile::copy(dir_to_phases, SS_DIR+"phases");
        } else {
            qDebug()<<"File not copied.!";
        }

    }

    // Saving the user inputs in the DB
    conn.connOpen();
    QSqlQuery qry;
    qry.prepare("UPDATE user_inputs"
                " SET ionu=:ionu, ior=:ior, iot=:iot, offset=:offset,"
                " phase1=:phase1, phasen=:phasen, nphase=:nphase, dinc=:dinc,"
                " dd=:dist, rmdx1=:rmdx1, rmdx2=:rmdx2, rmdy1=:rmdy1, rmdy2=:rmdy2,"
                " rmdz1=:rmdz1, rmdz2=:rmdz2, rmdz3=:rmdz3, rmdz4=:rmdz4, steps=:steps,"
                " stepsz=:sttepsz, gainx=:gainx, gainy=:gainy, gainz=:gainz, phasesfile=:phasesfile"
                " WHERE ID=2");
    qry.bindValue(":ionu", ionu);
    qry.bindValue(":ior", ior);
    qry.bindValue(":iot", iot);
    qry.bindValue(":offset", offset);
    qry.bindValue(":phase1", phase1);
    qry.bindValue(":phasen", phasen);
    qry.bindValue(":nphase", nphase);
    qry.bindValue(":dinc", dinc);
    qry.bindValue(":dist", dist);
    qry.bindValue(":rmdx1", rmdx1);
    qry.bindValue(":rmdx2", rmdx2);
    qry.bindValue(":rmdy1", rmdy1);
    qry.bindValue(":rmdy2", rmdy2);
    qry.bindValue(":rmdz1", rmdz1);
    qry.bindValue(":rmdz2", rmdz2);
    qry.bindValue(":rmdz3", rmdz3);
    qry.bindValue(":rmdz4", rmdz4);
    qry.bindValue(":steps", steps);
    qry.bindValue(":sttepsz", stepsz);
    qry.bindValue(":gainx", gainx);
    qry.bindValue(":gainy", gainy);
    qry.bindValue(":gainz", gainz);
    qry.bindValue(":phasesfile", ui->textAddPhases->text());

    // Executing the Query
    if (qry.exec()){
        qDebug()<<"LOS DB:"<<"Executed correctly."<< endl;
        qDebug()<<steps<<stepsz<<endl;
        conn.connClose();
    } else {
        qDebug()<<"LOS DB error:"<<qry.lastError().text()<<endl;
        qDebug()<<qry.executedQuery()<<endl;
        qDebug()<<qry.lastQuery()<<endl;
    }
}
void lineOfSight::set_default_user_inputs(QString field)
{
    QString id = "1";
    if (field == "user"){
        id = "2";
    } else if (field == "default"){
        id = "1";
    }

    // Load defaults DB values
    conn.connOpen();
    QSqlQueryModel qry;
    qry.setQuery("SELECT ionu,ior,iot,offset,phase1,phasen,nphase,dinc,"
                 "dd,rmdx1,rmdx2,rmdy1,rmdy2,rmdz1,rmdz2,rmdz3,rmdz4,steps,"
                 "stepsz,gainx,gainy,gainz, phasesfile FROM user_inputs WHERE ID="+id);
    qDebug()<<"LOS defaults DB errors: "<<qry.lastError()<<endl;

    ui->textionu->setText(qry.record(0).value("ionu").toString());
    ui->textior->setText(qry.record(0).value("ior").toString());
    ui->textiot->setText(qry.record(0).value("iot").toString());
    ui->textOffset->setText(qry.record(0).value("offset").toString());
    ui->textStartPhase->setText(qry.record(0).value("phase1").toString());
    ui->textEndPhase->setText(qry.record(0).value("phasen").toString());
    ui->textNoRotation->setText(qry.record(0).value("nphase").toString());
    ui->textIncAngle->setText(qry.record(0).value("dinc").toString());
    ui->textDistance->setText(QString::number(qry.record(0).value("dd").toDouble(), 'g', 7));
    ui->textx1->setText(qry.record(0).value("rmdx1").toString());
    ui->textx2->setText(qry.record(0).value("rmdx2").toString());
    ui->texty1->setText(qry.record(0).value("rmdy1").toString());
    ui->texty2->setText(qry.record(0).value("rmdy2").toString());
    ui->textz1->setText(qry.record(0).value("rmdz1").toString());
    ui->textz2->setText(qry.record(0).value("rmdz2").toString());
    ui->textz3->setText(qry.record(0).value("rmdz3").toString());
    ui->textz4->setText(qry.record(0).value("rmdz4").toString());
    ui->textSteps->setText(qry.record(0).value("steps").toString());
    ui->textStepsz->setText(qry.record(0).value("stepsz").toString());
    ui->textGainx->setText(qry.record(0).value("gainx").toString());
    ui->textGainy->setText(qry.record(0).value("gainy").toString());
    ui->textGainz->setText(qry.record(0).value("gainz").toString());
    ui->textAddPhases->setText(qry.record(0).value("phasesfile").toString());
    conn.connClose();
}

void lineOfSight::on_buttonAddPhases_clicked()
{
    if ( ui->textNoRotation->text().toInt() == 0) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textAddPhases->setText(directory);
    } else {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textAddPhases->setText(directory);
        QMessageBox msgBox;
        msgBox.warning(this, "Phases File", "If No. Phases is not 0 the Phases File will not have any effect.");
        msgBox.exec();
    }
}
