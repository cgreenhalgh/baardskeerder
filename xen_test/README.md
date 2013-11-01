to make test file:
'''
	dd bs=4096 count=25000 if=/dev/zero of=xen_testfile
'''

As of 2013-11-01 opam is not building xen-block-driver with blkback failing;
but configuing to build only blkfront and installing is OK. I also haven't
yet made a release incorporating the benchmark code in the main module
so you'll need to build and install the git HEAD version of 
mirage-baardskeerder.

Build with `mirari configure --xen` `mirari build --xen`; run with `sudo xl create -c bs_test.myxl`. If you don't see console output do `sudo xl create -p -c bs_test.myxl` and in another shell `sudo xl unpause bs-test`. Note that the xen_testfile has be re-generated before each run.
