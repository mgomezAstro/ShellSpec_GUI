#include "plotting_ss.h"
#include "ui_plotting_ss.h"
#include <QTextStream>
#include <QFile>
#include <QDebug>
#include <iostream>
#include <fstream>
#include <sstream>

#define R_SUN 69570000000.0


int cfort = 1;
QString prefix = "";

plotting_ss::plotting_ss(QWidget *parent) :
    QDialog(parent),
    ui(new Ui::plotting_ss)
{
    ui->setupUi(this);
    cfort = 1;
    ss.connOpen();
    QSqlQueryModel qry;
    qry.setQuery("SELECT prefix FROM user_inputs WHERE ID=2");
    prefix = qry.record(0).value("prefix").toString();
    ss.connClose();
//    qDebug()<<prefix;
    plott_example();
}

plotting_ss::~plotting_ss()
{
    delete ui;
}

void plotting_ss::plott_example()
{
//    shellspec ss;
//    QString prefix = ss.get_prefix_model();
    QString shellspectrum = prefix + "shellspectrum";
//    qDebug()<<shellspectrum;
    const QString SS_DIR = QDir::currentPath()+QDir::separator();

    plot_shellspectrum_file(SS_DIR + shellspectrum, false);
}

void plotting_ss::plot_fort_files(QString fortFile)
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    QString inFile = SS_DIR + fortFile;
    QFile file(inFile);
    QVector<double> zvalues;
    QVector<double> yvalues;
    QVector<double> xvalues;
    QVector<double> xrange(2);
    QVector<double> yrange(2);
    int nx = 0;
    int ny = 0;

    if (file.open(QIODevice::ReadOnly)){
        QTextStream st(&file);
        QString line = st.readLine();
        while (!line.isNull()){
            QStringList elems = line.split(QRegularExpression("\\s+"));
            if (elems.size() > 1){
                zvalues.append(elems.last().toDouble());
                xvalues.append(elems[1].toDouble());
                yvalues.append(elems[2].toDouble());
            }
            else if (elems.size() == 1){
                nx += 1;
                ny += 1;
            }
            line = st.readLine();
        }
    }

    // Defining plotting system
    clear_plot();
    ui->widgetPlot->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom);
    ui->widgetPlot->axisRect()->setupFullAxesBox(true);
    ui->widgetPlot->xAxis->setLabel("x[Rsun]");
    ui->widgetPlot->yAxis->setLabel("y[Rsun]");
    xrange[0] = xvalues[0] / R_SUN;
    xrange[1] = xvalues.last() / R_SUN;
    yrange[0] = yvalues[0] / R_SUN;
    yrange[1] = yvalues.last() / R_SUN;
    QCPColorMap *colorMap = new QCPColorMap(ui->widgetPlot->xAxis, ui->widgetPlot->yAxis);
    colorMap->data()->setSize(nx, ny);
    colorMap->data()->setRange(QCPRange(xrange[0], xrange[1]), QCPRange(yrange[0], yrange[1]));

    double x, y;
    int flag = 0;
    for (int xIndex = 0; xIndex < nx; ++xIndex) {
        for (int yIndex = 0; yIndex < ny; ++yIndex) {
            colorMap->data()->cellToCoord(xIndex, yIndex, &x, &y);
            colorMap->data()->setCell(xIndex, yIndex, zvalues[flag]);
            flag += 1;
        }
    }

    QCPColorScale *colorScale = new QCPColorScale(ui->widgetPlot);
    ui->widgetPlot->plotLayout()->addElement(0, 1, colorScale);
    colorScale->setType(QCPAxis::atRight);
    colorMap->setColorScale(colorScale);
    colorScale->axis()->setLabel("Intensity");
    colorMap->setGradient(QCPColorGradient::gpJet);
    colorMap->rescaleDataRange();

    QCPMarginGroup *marginGroup = new QCPMarginGroup(ui->widgetPlot);
    ui->widgetPlot->axisRect()->setMarginGroup(QCP::msBottom|QCP::msTop, marginGroup);
    colorScale->setMarginGroup(QCP::msBottom|QCP::msTop, marginGroup);

    ui->widgetPlot->rescaleAxes();
    ui->widgetPlot->yAxis->setScaleRatio(ui->widgetPlot->xAxis, 1.0);
    ui->widgetPlot->replot();
}

void plotting_ss::plot_lightcurve_file(QString lightFile)
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    QString lc = SS_DIR + lightFile;
    QFile file(lc);
    QVector<double> phase;
    QVector<double> velocity;
    QVector<double> magnitude;
    QVector<double> yrange(2);
    bool xcount = true;

    if (file.open(QIODevice::ReadOnly)){
        QTextStream st(&file);
        QString line = st.readLine();
        while (!line.isNull()){
            QStringList elems = line.split(QRegularExpression("\\s+"));
            if (elems.size() > 1){
                magnitude.append(elems[3].toDouble());
                if (xcount){
                    velocity.append(elems[2].toDouble());
                }
            }
            else if (elems.size() == 1){
                xcount = false;
                line = st.readLine();
                QStringList elems = line.split(QRegularExpression("\\s+"));
                if (elems.size() > 1){
                    magnitude.append(elems[3].toDouble());
                    phase.append(elems[1].toDouble());
                }
            }
            line = st.readLine();
        }
    }
    int nx = phase.size();
    int ny = velocity.size();

    clear_plot();
    // Defining plotting system
    ui->widgetPlot->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom);
    ui->widgetPlot->axisRect()->setupFullAxesBox(true);
    ui->widgetPlot->xAxis->setLabel("Phase");
    ui->widgetPlot->yAxis->setLabel("Velocity [km/s]");
    QCPColorMap *colorMap = new QCPColorMap(ui->widgetPlot->xAxis, ui->widgetPlot->yAxis);
    colorMap->data()->setSize(nx, ny);
    yrange[0] = *std::min_element(velocity.constBegin(), velocity.constEnd());
    yrange[1] = *std::max_element(velocity.constBegin(), velocity.constEnd());
    colorMap->data()->setRange(QCPRange(0, 1), QCPRange(yrange[0], yrange[1]));

    double x, y;
    int flag = 0;
    for (int xIndex = 0; xIndex < nx; ++xIndex) {
        for (int yIndex = 0; yIndex < ny; ++yIndex) {
            colorMap->data()->cellToCoord(xIndex, yIndex, &x, &y);
            colorMap->data()->setCell(xIndex, yIndex, magnitude[flag]);
            flag += 1;
        }
    }

    QCPColorScale *colorScale = new QCPColorScale(ui->widgetPlot);
    ui->widgetPlot->plotLayout()->addElement(0, 1, colorScale);
    colorScale->setType(QCPAxis::atRight);
    colorMap->setColorScale(colorScale);
    colorScale->axis()->setLabel("Magnitude [mag]");
    colorMap->setGradient(QCPColorGradient::gpJet);
    colorMap->rescaleDataRange();

    QCPMarginGroup *marginGroup = new QCPMarginGroup(ui->widgetPlot);
    ui->widgetPlot->axisRect()->setMarginGroup(QCP::msBottom|QCP::msTop, marginGroup);
    colorScale->setMarginGroup(QCP::msBottom|QCP::msTop, marginGroup);

    ui->widgetPlot->rescaleAxes();
    // ui->widgetPlot->yAxis->setScaleRatio(ui->widgetPlot->xAxis, 1.0);
    ui->widgetPlot->replot();
}

void plotting_ss::plot_shellspectrum_file(QString shellspecFile, bool overplot)
{
    shellspec ss;
    QString ssFile = shellspecFile;
    bool op = overplot;
    QVector<double> wave;
    QVector<double> flux;
    int ngraphs = 0;
    int nsize = 0;
    int ff_empty = 0;
    int total_ori_graphs = 0;

    QFileInfo check_file(ssFile);
    if (!check_file.exists() or !check_file.isFile()) {
        QMessageBox msgBox;
        msgBox.warning(this, "ShellSpectrum option.", "No such file or directory.");
    }

    QFile file(ssFile);
    if (file.open(QIODevice::ReadOnly)){
        QTextStream st(&file);
        QString line = st.readLine();
        while (!line.isNull()){
            QStringList elems = line.split(QRegularExpression("\\s+"));
            if (elems.size() > 1){
                wave.append(elems[1].toDouble());
                flux.append(elems[5].toDouble());
                if (ff_empty == 0){
                    nsize += 1;
                }
            } else {
                ff_empty = 1;
                ngraphs += 1;
            }
            line = st.readLine();
        }
    }

    // Preparing plot
    QPen pen;
    if (!op){
        clear_plot();
        pen = QPen(Qt::blue);
    } else if (op){
        pen = QPen(Qt::red);
        total_ori_graphs = ss.count_total_graphs_current_model();
    }


    ui->widgetPlot->axisRect()->setupFullAxesBox(true);
    ui->widgetPlot->xAxis->setLabel("Wavelength [Ang]");
    ui->widgetPlot->yAxis->setLabel("Normilized Flux");

    connect(ui->widgetPlot->xAxis, SIGNAL(rangeChanged(QCPRange)), ui->widgetPlot->xAxis2, SLOT(setRange(QCPRange)));
    connect(ui->widgetPlot->yAxis, SIGNAL(rangeChanged(QCPRange)), ui->widgetPlot->yAxis2, SLOT(setRange(QCPRange)));

    int flag = 0;
    for (int i = total_ori_graphs; i < ngraphs + total_ori_graphs; ++i) {
        QVector<double> tmpx;
        QVector<double> tmpy;
        for (int n = 0; n < nsize; ++n){
            tmpx.append(wave[flag]);
            tmpy.append(flux[flag]);
            flag += 1;
        }
        ui->widgetPlot->addGraph();
        ui->widgetPlot->graph(i)->setPen(pen);
        ui->widgetPlot->graph(i)->setData(tmpx, tmpy);
    }
    ui->widgetPlot->xAxis->setRange(QCPRange(wave[0], wave.last()));
    double miny = *std::min_element(flux.constBegin(), flux.constEnd());
    ui->widgetPlot->yAxis->setRange(QCPRange(miny, 1.0));
    ui->widgetPlot->setInteractions(QCP::iRangeDrag | QCP::iRangeZoom | QCP::iSelectPlottables);
    ui->widgetPlot->replot();

}

void plotting_ss::on_buttonPlotSave_clicked()
{
    const QString SS_DIR = QDir::currentPath()+QDir::separator();
    QString savePath = QFileDialog::getSaveFileName(this, "Save file...", "", ".pdf");
    ui->widgetPlot->savePdf(savePath, ui->widgetPlot->width(), ui->widgetPlot->height());
}

void plotting_ss::on_radioLightcurve_toggled(bool checked)
{
    QString lcFile = prefix + "lightcurve";
//    qDebug()<<lcFile;
    if (checked){
        plot_lightcurve_file(lcFile);
    }
}

void plotting_ss::clear_plot()
{
    ui->widgetPlot->clearPlottables();
    while ( ui->widgetPlot->plotLayout()->elementCount() != 1){
        ui->widgetPlot->plotLayout()->removeAt(1);
        ui->widgetPlot->plotLayout()->simplify();
    }
}

void plotting_ss::on_radio2Dphases_toggled(bool checked)
{
    qDebug()<<prefix;
    QString cc = tr("%1").arg(1, 3, 10, QChar('0'));
    QString fortFile = prefix + "2Dimage_" + cc;
    qDebug()<<fortFile;
    if (checked){
        plot_fort_files(fortFile);
    }
    if (cfort == 1){
        ui->buttonPlotBack->setEnabled(false);
    }
}

void plotting_ss::on_radioShellspectrum_toggled(bool checked)
{
    bool overplot = false;
    const QString SS_DIR = QDir::currentPath()+QDir::separator();

    if (checked){
//            qDebug()<<SS_DIR + prefix + "shellspectrum";
            plot_shellspectrum_file(SS_DIR + prefix + "shellspectrum", overplot);
    }
}

void plotting_ss::on_buttonOP_clicked()
{
    if ( ui->checkOP->isEnabled() ) {
        QString directory = QFileDialog::getOpenFileName(this, tr("Find Files"), QDir::currentPath());
        ui->textOP->setText(directory);
    } else {
    QMessageBox msgBox;
    msgBox.setText("Active ShellSpectrum first!.");
    msgBox.exec();
    }
}

//int plotting_ss::count_total_graphs_current_model()
//{
//    const QString SS_DIR = QDir::currentPath()+QDir::separator();
//    shellspec ss;
//    QString prefix = ss.get_prefix_model();
//    qDebug()<<"plotting:"<<prefix;
//    int total_graphs = 0;
//    QString ssFile = SS_DIR + prefix + "shellspectrum";
//    qDebug()<<ssFile;
//    QFile file(ssFile);
//    if (file.open(QIODevice::ReadOnly)){
//        QTextStream st(&file);
//        QString line = st.readLine();
//        while (!line.isNull()){
//            QStringList elems = line.split(QRegularExpression("\\s+"));
//            if (elems.size() == 1){
//                total_graphs += 1;
//            }
//            line = st.readLine();
//        }
//    }
//    return total_graphs;
//}

void plotting_ss::on_buttonPlot_clicked()
{
    if (ui->checkOP->isChecked()){
        QString overplotFile = ui->textOP->text();
        plot_shellspectrum_file(overplotFile, true);
    }
}


void plotting_ss::on_buttonPlotBack_clicked()
{
    cfort -= 1;
    if (cfort == 1){
        ui->buttonPlotBack->setEnabled(false);
        QString currentFort = tr("%1").arg(cfort, 3, 10, QChar('0'));
        qDebug()<<currentFort;
        plot_fort_files(prefix + "2Dimage_" + currentFort);
        // QString prefix = ss.get_prefix_model();
    } else {
        if (!ui->buttonPlotNext->isEnabled()){
            ui->buttonPlotNext->setEnabled(true);
        }
        QString currentFort = tr("%1").arg(cfort, 3, 10, QChar('0'));
        qDebug()<<currentFort;
        plot_fort_files(prefix + "2Dimage_" + currentFort);
        // QString prefix = ss.get_prefix_model();
    }
}

void plotting_ss::on_buttonPlotNext_clicked()
{
    int nfort = ss.count_total_graphs_current_model();
    qDebug()<<nfort;
    cfort += 1;

    if (cfort == nfort){
        ui->buttonPlotNext->setEnabled(false);
        QString currentFort = tr("%1").arg(cfort, 3, 10, QChar('0'));
        qDebug()<<currentFort;
        plot_fort_files(prefix + "2Dimage_" + currentFort);
    } else {
        if (!ui->buttonPlotBack->isEnabled()){
            ui->buttonPlotBack->setEnabled(true);
        }
        QString currentFort = tr("%1").arg(cfort, 3, 10, QChar('0'));
        qDebug()<<currentFort;
        plot_fort_files(prefix + "2Dimage_" + currentFort);
    }
}
