
#include "sim_feedback_impl.hh"

SimFeedbackImpl::SimFeedbackImpl(QWidget* parent,  
				 const char* name, 
				 WFlags fl)
  : SimFeedbackForm( parent, name, fl )
{
}

SimFeedbackImpl::~SimFeedbackImpl()
{
  // no need to delete child widgets, Qt does it all for us
}
