
#ifndef FLOAT_SPIN_BOX_HH_INCLUDED
# include "float_spin_box.hh"
#endif
#ifndef TRAIT_OR_SNP_TABLE_HH_INCLUDED
# include "trait_or_snp_table.hh"
#endif

#ifndef QWIDGETPLUGIN_H_INCLUDED
# include <qwidgetplugin.h>
# define QWIDGETPLUGIN_H_INCLUDED
#endif

class CustomWidgetPlugin : public QWidgetPlugin
{
public:
    CustomWidgetPlugin();

    QStringList keys() const;
    QWidget* create( const QString &classname, 
		     QWidget* parent = 0, const char* name = 0);
    QString group(const QString&) const;
    //QIconSet iconSet(const QString&) const;
    QString includeFile(const QString&) const;
    QString toolTip(const QString&) const;
    QString whatsThis(const QString&) const;
    bool isContainer(const QString&) const;
};

CustomWidgetPlugin::CustomWidgetPlugin()
{
}

QStringList
CustomWidgetPlugin::keys() const
{
    QStringList list;
    list << "FloatSpinBox";
    list << "TraitOrSNPTable";
    return list;
}

QWidget*
CustomWidgetPlugin::create(const QString &key, 
			   QWidget* parent, const char* name )
{
    if ( key == "FloatSpinBox" )    return new FloatSpinBox( parent, name );
    if ( key == "TraitOrSNPTable" ) return new TraitOrSNPTable( parent, name );
    return 0;
}

QString
CustomWidgetPlugin::includeFile(const QString& feature) const
{
    if ( feature == "FloatSpinBox" )    return "float_spin_box.hh";
    if ( feature == "TraitOrSNPTable" ) return "trait_or_snp_table.hh";
    return QString::null;
}

QString
CustomWidgetPlugin::group(const QString& /*feature*/) const
{
    return "CoaSim Custom Widgets";
}


#if 0 
QIconSet
CustomWidgetPlugin::iconSet( const QString& ) const
{
    return QIconSet(QPixmap(float_spin_box_pixmap));
}
#endif

QString
CustomWidgetPlugin::toolTip( const QString& feature ) const
{
    if ( feature == "FloatSpinBox" ) 
	return "Spin box for floats";
    if ( feature == "TraitOrSNPTable" ) 
	return "Table for specifying trait or SNP markers";
    return QString::null;
}

QString
CustomWidgetPlugin::whatsThis(const QString& feature) const
{
    if ( feature == "FloatSpinBox" )
	return "A spin box widget for floats";
    if ( feature == "TraitOrSNPTable" ) 
	return "Table for specifying trait or SNP markers";
    return QString::null;
}

bool 
CustomWidgetPlugin::isContainer( const QString& ) const
{
    return FALSE;
}

Q_EXPORT_PLUGIN( CustomWidgetPlugin )
