%%-*- mode: erlang -*-
{application, erlawys,
 [
  {description, "erlawys"},
  {vsn, "1"},
  {modules, [
	aws_ec2,
	aws_ec2_test,
	aws_ec2_xml,
	aws_fps,
	aws_fps_xml,
	aws_util
            ]},
  {registered, []},
  {applications, [
                  kernel,
                  stdlib,
                  crypto
                 ]},
  {mod, { erlawys, []}},
  {env, []}
 ]}.

