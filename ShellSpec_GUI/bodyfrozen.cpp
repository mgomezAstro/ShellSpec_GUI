#include "bodyfrozen.h"
#include "ui_bodyfrozen.h"

bodyfrozen::bodyfrozen(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::bodyfrozen)
{
    ui->setupUi(this);
    QString field = "user";
    set_default_user_inputs(field);
}

bodyfrozen::~bodyfrozen()
{
    delete ui;
}

void bodyfrozen::on_buttonBox_accepted()
{
    int imodel = 1;
    QString rmdfx1 = ui->textx1->text();
    QString rmdfx2 = ui->textx2->text();
    QString rmdfy1 = ui->texty1->text();
    QString rmdfy2 = ui->texty2->text();
    QString rmdfz1 = ui->textz1->text();
    QString rmdfz2 = ui->textz2->text();
    QString rmdfx3 = ui->textx3->text();
    QString rmdfx4 = ui->textx4->text();
    QString stepf = ui->textSteps->text();
    QString stepfz = ui->textStepsz->text();
    QString gainfx = ui->textGainx->text();
    QString gainfy = ui->textGainy->text();
    QString gainfz = ui->textGainz->text();

    // Saving the user inputs in the DB
    conn.connOpen();
    QSqlQuery qry;
    qry.prepare("UPDATE user_inputs"
                " SET imodel=:imodel, rmdfx1=:rmdfx1, rmdfx2=:rmdfx2, rmdfy1=:rmdfy1, rmdfy2=:rmdfy2,"
                " rmdfz1=:rmdfz1, rmdfz2=:rmdfz2, rmdfx3=:rmdfx3, rmdfx4=:rmdfx4, stepf=:stepf,"
                " stepfz=:sttepfz, gainfx=:gainfx, gainfy=:gainfy, gainfz=:gainfz"
                " WHERE ID=2");
    qry.bindValue(":imodel", imodel);
    qry.bindValue(":rmdfx1", rmdfx1);
    qry.bindValue(":rmdfx2", rmdfx2);
    qry.bindValue(":rmdfy1", rmdfy1);
    qry.bindValue(":rmdfy2", rmdfy2);
    qry.bindValue(":rmdfz1", rmdfz1);
    qry.bindValue(":rmdfz2", rmdfz2);
    qry.bindValue(":rmdfx3", rmdfx3);
    qry.bindValue(":rmdfx4", rmdfx4);
    qry.bindValue(":stepf", stepf);
    qry.bindValue(":sttepfz", stepfz);
    qry.bindValue(":gainfx", gainfx);
    qry.bindValue(":gainfy", gainfy);
    qry.bindValue(":gainfz", gainfz);

    // Executing the Query
    if (qry.exec()){
        qDebug()<<"LOS DB:"<<"Executed correctly."<< endl;
        conn.connClose();
    } else {
        qDebug()<<"LOS DB error:"<<qry.lastError().text()<<endl;
        qDebug()<<qry.executedQuery()<<endl;
        qDebug()<<qry.lastQuery()<<endl;
    }
}
void bodyfrozen::set_default_user_inputs(QString field)
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
    qry.setQuery("SELECT imodel,rmdfx1,rmdfx2,rmdfy1,rmdfy2,rmdfz1,rmdfz2,rmdfx3,rmdfx4,stepf,"
                 "stepfz,gainfx,gainfy,gainfz from user_inputs WHERE ID="+id);
    qDebug()<<"LOS defaults DB errors: "<<qry.lastError()<<endl;

    ui->textx1->setText(qry.record(0).value("rmdfx1").toString());
    ui->textx2->setText(qry.record(0).value("rmdfx2").toString());
    ui->texty1->setText(qry.record(0).value("rmdfy1").toString());
    ui->texty2->setText(qry.record(0).value("rmdfy2").toString());
    ui->textz1->setText(qry.record(0).value("rmdfz1").toString());
    ui->textz2->setText(qry.record(0).value("rmdfz2").toString());
    ui->textx3->setText(qry.record(0).value("rmdfx3").toString());
    ui->textx4->setText(qry.record(0).value("rmdfx4").toString());
    ui->textSteps->setText(qry.record(0).value("stepf").toString());
    ui->textStepsz->setText(qry.record(0).value("stepfz").toString());
    ui->textGainx->setText(qry.record(0).value("gainfx").toString());
    ui->textGainy->setText(qry.record(0).value("gainfy").toString());
    ui->textGainz->setText(qry.record(0).value("gainfz").toString());

    conn.connClose();
}
