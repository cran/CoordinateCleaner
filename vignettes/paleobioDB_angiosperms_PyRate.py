#!/usr/bin/env python
from numpy import * 


data_1=[array([15.13001]),
array([64.441966,64.368785,63.440808,0]),
array([58.221045,0]),
array([65.799171,0]),
array([67.813798,0]),
array([13.787084,13.982968,12.09956,13.706748,15.916955,12.079826,13.901296,13.550628,14.521793,15.481413,0]),
array([62.635233,64.116479,54.043646,0]),
array([65.760506]),
array([50.739624,0]),
array([65.73031,62.332822,64.232145,63.676424]),
array([40.053137,38.335287,48.518137,40.164144,50.428151,47.862152]),
array([47.817422,0]),
array([50.449351,0]),
array([22.40045]),
array([60.429137]),
array([59.751963,0]),
array([44.328714]),
array([40.571197,0]),
array([48.143476,47.149049,42.408386,54.497439]),
array([44.093507,45.221111]),
array([49.85244,38.990988,37.826794,46.092376,54.416614,48.394592,48.367513,44.056605,34.549278,46.256744,45.345644,43.479165,48.201531,49.52502,42.887381,45.998817,48.789589,35.903651,38.380202,0]),
array([57.455777,56.280869,58.228553,56.100613,58.462957,56.49022,56.471514,57.739068,55.831417,58.19401,0]),
array([43.939519,44.436595,44.105314,0]),
array([55.837377,55.594114,53.419401,44.207351,43.85315,43.411333]),
array([58.631269,46.263409,52.110572,51.4103,50.983336,0]),
array([53.27853,51.80659,40.271896,41.594967]),
array([66.698213]),
array([57.740196,62.383199,65.065105,0]),
array([99.441965]),
array([111.622656,0]),
array([64.962771,64.780739,59.542672,62.128277,62.100156,61.634915,61.73781,65.823833]),
array([110.675325,106.808792,103.780188,103.714442]),
array([103.474358]),
array([82.749903]),
array([104.168417,0]),
array([75.39649,0]),
array([73.943725,0]),
array([74.216584,0]),
array([78.651105,0]),
array([120.627157,0]),
array([63.492813,0]),
array([42.101494,0]),
array([45.250906,45.513834,45.023467,38.174117,43.676201,38.939838,45.584901,44.336152,47.779299,47.104644,42.987291,53.080168,50.658359,55.061947,44.00465,40.682129,0]),
array([46.766075]),
array([71.379236]),
array([57.500716]),
array([55.223934,0]),
array([55.008335,0]),
array([54.043075,0]),
array([50.771108]),
array([52.78335,0]),
array([49.556525]),
array([30.928789,17.280043,0]),
array([53.510885,0]),
array([49.178823,0]),
array([55.365747,0]),
array([48.854424,0]),
array([58.669225,58.651018,56.201303,56.929951,57.290284,58.564952,0]),
array([22.043406,21.440959,50.373389]),
array([58.248086,57.217354,51.681698]),
array([64.121503]),
array([36.838401]),
array([62.290902,0]),
array([69.444755,0]),
array([51.069599]),
array([97.627587,0]),
array([94.721404,0]),
array([56.319851,56.497004,65.577941,61.787776,62.377478,62.965308,65.409647,65.539808,65.673576,64.41471,64.316867,70.275578,68.866521,68.423321,70.530516,67.580497,69.283528,64.033125,63.643108,67.628267,63.51013,66.041542,64.792785,65.530516,65.960098,64.805458,64.949465,64.817659,70.449626,61.10112,64.363709,61.861749,59.520007,58.124387,59.233276,65.561096,56.180491,64.241873,65.743567,63.569807,66.032589,67.164056,66.504918,66.586973,0]),
array([41.87283,0]),
array([49.675436,0]),
array([54.998058,0]),
array([13.899765,0]),
array([70.309922,81.270451,0]),
array([97.56018]),
array([55.389039]),
array([51.327206,0]),
array([36.325853,34.470512,35.399508,34.038328,35.608857,35.074443,36.948266,34.564323,0]),
array([135.348797]),
array([56.559957,56.778619,55.966181]),
array([56.044581]),
array([95.671356]),
array([56.449457]),
array([110.404468]),
array([49.073761,0]),
array([32.154845,0]),
array([39.944555]),
array([53.483864,0]),
array([98.864182,0]),
array([50.990659,109.106451,107.992377,102.587079,111.139566,111.64149,75.728327,82.232367,89.363753,29.322295,0]),
array([96.569963,0]),
array([110.082427,29.431932,73.089775,0]),
array([29.039603,72.126986]),
array([28.451564,0]),
array([33.055436]),
array([28.589314]),
array([30.508191]),
array([28.34216,0]),
array([30.698459,0]),
array([31.275461]),
array([110.807636,28.906692,69.676046,0]),
array([109.5743,31.828989,70.122578]),
array([109.670791,32.400252]),
array([31.421431,0]),
array([32.482255]),
array([28.308448,0]),
array([31.585072,69.479742]),
array([32.002865,74.705413]),
array([106.493227]),
array([98.340589]),
array([80.724747,68.919928]),
array([33.578678,0]),
array([106.702331]),
array([61.602891]),
array([56.051667,0]),
array([38.022241,0]),
array([14.360899,0]),
array([38.347739]),
array([11.445957,0]),
array([62.196634,0]),
array([43.304635]),
array([51.544235,45.993954,46.165413,0]),
array([55.831497,49.503669,54.526157,46.379447,0]),
array([56.521695,58.065323,55.87722,57.063624,67.412787]),
array([109.381116,0]),
array([43.124082,37.230403,44.151802,43.522779,38.056262,45.725823,47.109595,37.652957,38.54882,38.133084,41.033178,0]),
array([38.844672]),
array([48.330016,47.95536,44.400217,42.106178,38.520957,48.118765]),
array([39.009595]),
array([51.122579,35.135314]),
array([62.393534,0]),
array([53.859724,36.367177,36.029333,35.57288,34.274956,34.499534,44.14265,45.143784,54.144668,38.06672,36.381211,53.093875,47.011066,47.518205,47.729677,49.860452,40.187307,34.002163,43.210066,37.478041,43.63507,51.354663,47.080758,52.182313,46.743605,53.026001,35.089974,40.765318,39.328634,54.814161,52.157382,48.418127,52.47152,0]),
array([55.198969,0]),
array([23.745076,0]),
array([4.858223]),
array([4.092907]),
array([67.68602]),
array([63.185588,64.849885]),
array([81.312884,62.007458,70.776559,66.900256,35.167262,0]),
array([72.07631,72.086382,0]),
array([33.983228]),
array([53.860702,0]),
array([82.016781,0]),
array([41.684435,40.797243,37.604585,0]),
array([53.32733,45.163164]),
array([31.447618,0]),
array([41.304628]),
array([45.170235,41.671829,41.355469,46.354465,48.530917,0]),
array([58.330058,51.124705,40.521536,46.478768,0]),
array([88.099961,0]),
array([53.931551,54.166792,45.430776]),
array([40.113374,46.296452,42.569997,40.401712,46.744761,39.708401,47.665237,39.358464,53.550286,52.164331,51.293211,39.419874,44.972123,51.044156,52.883473,37.846928,54.731794,52.823944,49.175593,51.567291,53.822171,49.048055,51.430028,48.959353,49.625315,55.436884,53.359654,50.472627,54.927517,55.394598,48.61143,50.928168]),
array([39.024089,50.794374,0]),
array([19.435603]),
array([56.83018]),
array([54.892921,50.839106,54.154953,51.864787,49.042035,55.464023,50.29003,49.756898,49.104122,0]),
array([34.269752]),
array([56.071988,28.188597,77.954757,36.435991,0]),
array([57.487699,56.313549,0]),
array([49.995661,53.599093,42.671574,0]),
array([54.689925,42.442725,42.181527,0]),
array([58.126311,57.12974]),
array([45.639064,0]),
array([64.121183,61.355386,56.100178,69.874368]),
array([37.287345]),
array([48.512522,47.0604]),
array([65.907988,0]),
array([49.070426,52.240461,52.951199,55.465771,49.323526,49.236452,54.79492,54.721953,53.898667,54.540564,49.783528,50.78432,52.907748,52.369843,53.921498,53.95589,41.879184,0]),
array([77.141955]),
array([52.609638,51.465186,44.020118,46.968705]),
array([94.096503]),
array([65.986057,64.572213,63.286499,65.647238,62.424866,63.505676]),
array([94.09131,0]),
array([64.233005,63.026056,63.311353,0]),
array([64.203417,62.239392]),
array([56.519171,0]),
array([51.886809]),
array([12.388132]),
array([0.211829]),
array([3.261393]),
array([0.963008]),
array([67.576058,65.966647,64.450456,63.090455,63.880919,61.821733,64.509908,65.342611,64.043422,64.611756,62.421125,20.657506,35.096661,47.607886,38.925686,45.104864,36.845282,37.072804,63.834142,64.329934,56.166883,56.473245,56.368524,57.856602,57.382836,57.967823,32.789513,31.002897,50.143186,50.318451,16.091036,49.263476,65.126999,63.457689,49.415545,61.863149,36.494895,15.375847,15.743777,13.159596,12.399459,13.224659,14.782638,14.916764,12.578938,12.611536,12.431588,14.229462,15.870384,15.292049,12.787228,14.542364,17.245758,18.313705,29.837155,30.661629,19.616429,12.731127,0]),
array([21.888108,0]),
array([18.898095,0]),
array([62.992927,0]),
array([10.739591,19.318556,28.382235,67.935576,70.221321,27.268874,54.919495,0]),
array([57.863325,50.275277,48.740897,44.115883,44.512331,44.594812,42.182221,38.736042,46.691126,38.500202,0]),
array([96.923578,95.717278,108.711249,108.284386]),
array([97.762211,0]),
array([44.766577,41.894858,38.693135,46.398526,41.084077,39.438922,39.542631,0]),
array([108.830689,0]),
array([110.366161]),
array([105.767126]),
array([44.313484,47.238007,40.99482]),
array([44.399541,38.081249,39.204173,0]),
array([55.332232,69.847422,0]),
array([70.475346]),
array([48.837821,48.294507,50.685139,47.918376]),
array([52.011942,48.76205,55.149829,55.416806,54.114055,48.901589,49.339499,52.661898,51.026841,55.160352,48.98804,55.53772]),
array([54.710075]),
array([52.976936,50.652199,55.010479,50.125948,51.047576,49.10127,53.833874,49.822377,48.932801,52.889394,48.373351,52.450169,54.490981,51.624864]),
array([49.913301,53.922058,48.482282,52.672294,49.890058,49.997839,53.200549,47.92482,49.688758,54.783911,0]),
array([49.970283,50.509651,49.093899,52.776372,50.505106,0]),
array([51.845622,48.593512,51.826228,55.544592,48.255224,55.8763,54.737108,48.027195,54.743476,50.566973,55.844884,55.329692,52.963084,48.843423,54.865163]),
array([53.327141,49.604352,54.207669,53.805658,49.408164,50.721369,53.481173,55.530944,50.764414,52.135322,0]),
array([53.580474,49.253137,52.841519,50.83229,55.175482,48.419978,49.852399,52.946745,50.859445,53.619331,55.068215,53.738421,53.462272,50.428354,51.397771,0]),
array([48.143613,0]),
array([55.528731]),
array([52.701662,49.211053,52.58608,55.95494]),
array([52.117401,52.94917]),
array([52.655603,0]),
array([52.019116,0]),
array([54.072722,49.736283,50.36825,0]),
array([109.189357]),
array([97.608521,0]),
array([15.830618,0]),
array([11.299795]),
array([34.856988,0]),
array([9.825174,0]),
array([51.983341,34.570864]),
array([41.077497,35.434782,36.429829,36.607072,36.776073,34.024949,34.967479,35.310325,0]),
array([1.464789,1.980754,1.639209,0.963992,1.590625,1.309872,2.406163,1.333606,1.971,1.901935,1.310723,0]),
array([32.295766]),
array([50.020768,0]),
array([51.592122,48.980034,41.295067,36.326964,36.284025,34.857854,35.879127,36.056882,34.310649,34.423795,36.753016,34.785656,34.986931,0]),
array([39.168727,0]),
array([43.991127]),
array([46.3787]),
array([70.124446,0]),
array([21.057443]),
array([36.472218,34.243203,48.791541,48.714314,50.479509,52.408766,55.320442,48.615355,53.439175,48.87578,50.886941,0]),
array([19.792244,57.713317,58.30962,31.518661,49.754148,17.70565,38.104346,34.687467,15.177432,15.871899,14.676778,15.742417,14.613296,12.756387,15.775823,21.878605,21.17377,18.043039,31.500091,64.950588,57.746436,62.050557,60.791707,62.019028,58.586943,56.687095,57.3018,59.337903,65.71973,44.78026,35.313524,62.594201]),
array([66.879197]),
array([81.103495,0]),
array([49.307878,0]),
array([62.532857,0]),
array([65.158225]),
array([36.718748,34.495319,35.434815,34.800852,36.77939,35.332154,34.204719,35.805377,37.124626,35.063515,33.921299,35.370619,36.002088,36.380952,34.072724,35.237331,0]),
array([93.50887,0]),
array([78.635504]),
array([70.05299,64.068389,63.916792,70.796629,79.530814,69.794015,0]),
array([43.640705]),
array([98.702441]),
array([43.333206]),
array([40.863943]),
array([30.132751]),
array([96.818088,93.955848,102.529686,0]),
array([62.902275,58.798266,0]),
array([6.325634]),
array([39.513249,15.645196,0]),
array([18.450933,57.285513,58.277059,57.822912,57.073307,58.668323,30.999991,26.066083,53.580996,18.355385,71.450601,67.659922,35.965339,33.894949,31.931739,22.463129,34.11307,53.837634,15.06595,0]),
array([64.96929,0]),
array([56.18605,56.782214,57.770833,56.610728,56.025035,58.145372,58.076323,55.937017,56.74847,28.409716,51.185843,19.811407,43.62531,35.465608,67.4769]),
array([53.311469,0]),
array([56.816349,0]),
array([71.556542,75.139301,98.136726]),
array([34.465161]),
array([35.821044]),
array([25.950654,0]),
array([10.762081,22.703445,30.758543,36.497291]),
array([51.866474]),
array([109.090956,40.925456,66.952474,70.466408,86.260495]),
array([75.668772,78.934794]),
array([56.672299,58.160808,58.623742,0]),
array([5.701152,0]),
array([47.552948,0]),
array([57.965399]),
array([19.400313]),
array([73.286816,80.437434,62.876724,57.311007,55.947317,56.390414,55.860614,56.091113,58.587115,56.878912,57.085711,81.924373,62.744223,0]),
array([70.097166,67.554976]),
array([68.937921,21.760484,51.552038,0]),
array([62.124062]),
array([65.552105]),
array([61.716947,0]),
array([1.61091,1.007735,1.913828,1.401938,1.259803,2.322836,0.891736]),
array([0.328346]),
array([57.967913,0]),
array([56.047869]),
array([3.022095]),
array([65.63227,64.300418,0]),
array([65.85546,0]),
array([21.344397,0]),
array([49.303852,50.621633,53.882366,54.614126,50.810006,52.645772,49.441296,51.319115,49.858982,53.286337,52.287068,52.001382,51.0064,50.207143,53.718284,50.828539,50.78053,55.599162,53.948816,55.66629,51.369716,50.790133,49.329848,51.099364,49.116975,52.440582]),
array([5.524333]),
array([89.001669,0]),
array([108.798563,102.216525,121.76905,106.873218,0]),
array([18.150528,0]),
array([41.651097,47.819413]),
array([51.369789,51.160738,0]),
array([52.40198,0]),
array([29.038529]),
array([56.261291,61.087369,65.953687,61.824428,63.402786,0]),
array([64.193998,64.788037,65.83934,61.890424,61.845674,65.104457,62.205205,63.211374,61.800307,61.894809,65.641037]),
array([101.031324,103.644372,109.672726,99.041312,96.35475,95.010882,96.873313,98.253467,96.998986,96.300825,101.661654,66.766192,56.797511,58.128819,57.291317,65.30224,57.452796,58.870843,64.834439,57.992108,62.264257,61.095913,64.734514,63.719914,63.376853,62.813524,60.402875,61.787409,58.620137,57.846192,57.230437,56.518208,56.22931,57.342198,57.01437,56.368471,56.011058,58.507577,56.577199,57.899577,56.391561,55.578283,53.209679,50.131156,52.477252,55.788018,55.594093,50.656498,99.071421,62.218691,72.447112,53.247219,49.499497,64.067327,63.942618,35.237375,34.591354]),
array([70.588129,67.030676,66.128852,63.67792,61.929922,64.527007,63.598725,64.16224,63.135785,65.104421,63.300855,62.553536,64.604032,69.023809]),
array([66.489714,65.363659,63.966692]),
array([63.165104,62.360612]),
array([63.65267,0]),
array([85.840701,0]),
array([53.531995,0]),
array([52.073925,51.670005,48.747677,50.078632,52.961311,49.312838,54.165087,0]),
array([49.082334,0]),
array([55.857473,57.328411,56.64396,56.112659,48.740651,51.237351,55.066098,49.634742,53.837034,52.601282,48.686465,53.70174,53.087318,53.006683,51.129037,48.808806,50.60443,0]),
array([71.220163,68.487077,70.914032,68.411101,0]),
array([62.512363,64.95854,64.006447,64.091546,68.751125,69.621903,67.135044,64.638655,65.781666,67.211047,66.47584,70.446739,67.329972,70.24616,66.747215,69.349176,0]),
array([76.609186]),
array([50.284871,52.927652,46.813493,48.21286,0]),
array([43.939543,0]),
array([56.642529,49.339879,0]),
array([65.283482,61.970365,65.665812,63.719907,62.51518,64.648215,63.724506,64.320311,61.600256,62.410316,63.193711,62.690824,65.129437,63.801076,61.862773,64.01438,64.791009,62.885182,61.644471,65.215592,64.397183,62.205828,62.120871,0]),
array([63.473297,0]),
array([63.700358,0]),
array([37.757872]),
array([38.367369,42.651671]),
array([6.458009,0]),
array([122.07538,100.932946,0]),
array([55.601582,55.424871,44.356764,42.275907]),
array([48.968399,54.703228,41.896474]),
array([22.103144,0]),
array([51.493004,54.739519,44.452747,0]),
array([53.309982,54.038069,47.477722,39.053354,0]),
array([53.054053,49.00035,37.503777,42.309851]),
array([66.150951,69.241565,81.483276,68.033411,0]),
array([64.285538]),
array([63.613005,63.187737,65.02467,65.565269]),
array([30.158438,0]),
array([51.37202,0]),
array([36.57075,0]),
array([57.065374,52.103291,52.777523,46.543664,0]),
array([47.594373,45.752842,92.493833,63.222241,31.147697,66.535069,70.172,70.347779,70.476845,66.724087,79.564458,82.991292,79.496419,98.294558,42.068242,0]),
array([64.663381,64.815722,62.760446,62.177823,65.652973,63.872165,61.666128,64.406261,65.859864,64.916435,64.446644,0]),
array([71.214838,135.749297]),
array([50.525254,48.81959,51.705783,53.168056,54.472377,52.634654,49.379935,0]),
array([39.231379,46.539413,43.89176,42.355068,0]),
array([20.980282,22.564712,26.943816]),
array([7.340199,14.007151,15.501326,12.149253,15.902717,13.661865,22.341565,2.423907,2.346362,1.53274,1.850488,2.461392,0.90259,1.169729,0.923646,2.383281,2.520416,2.481088,2.203423,1.804944,1.23666,1.036799,1.778732]),
array([63.175227,57.078039,56.302405,60.591717,64.511337,58.564647,64.511878]),
array([26.865841]),
array([10.475667,22.435363,26.080455,28.016173,17.79741,14.348053,23.629442,36.058038,63.410832,62.851558,63.975518,65.295404,64.343715,62.627319,64.374134,63.420752,63.724981,62.259099,62.500999,65.7289,63.163387,64.022123,61.966978,70.003227,70.919023,38.495902,49.877097,62.142439]),
array([62.291971,0]),
array([70.434983]),
array([47.455542,37.83815]),
array([38.99495,0]),
array([23.515963,24.755854,23.16633,25.956471,27.56657,17.050969,16.408714,17.94706,16.344569,25.006914,28.15692]),
array([16.322215,13.70404,12.090674,14.652383]),
array([11.104119]),
array([46.080148,41.430972,48.442216,39.85056,47.140471,47.996377,48.332886,39.641969,47.506902,40.915922,44.38704,40.929321,0]),
array([44.721461,40.421606,43.656895,40.721849,45.631901,47.735769]),
array([1.757064,2.051255]),
array([9.099144]),
array([51.671044]),
array([66.937052,65.517308,0]),
array([69.915256,68.844487,64.837199,65.512683,63.889339,62.493383,64.908538,61.750503,63.105849,62.395602,65.596639,65.471507,63.090948,63.584279,65.487126,61.786395,64.402205,64.721282,68.306453,71.701993,54.832404,65.856788,0]),
array([63.22974,0]),
array([35.188521,0]),
array([19.109609,24.590744,23.757331,23.926904,16.241533,49.71021,65.46541,0]),
array([8.234964,0]),
array([37.620774,25.578238,14.695151,71.403809,76.037424,49.225805,60.292322,62.877881,73.567846,25.233754,69.009494,95.350072,0]),
array([71.402177,0]),
array([65.528113,63.033286,64.961801,62.077969,64.858457,65.439166,63.475722,65.089757,61.860386,0]),
array([72.200096,35.823483]),
array([35.700984]),
array([35.303164]),
array([34.031796]),
array([20.337894,18.414139,19.253295,26.965166,51.112912,0]),
array([8.702736,65.584411,64.246055,49.322206,53.899226]),
array([62.621854]),
array([114.170702]),
array([115.251904,115.670927,119.43844]),
array([114.926893,0]),
array([104.889644,0]),
array([106.891921,107.74432,95.339341,105.760898]),
array([120.942551,105.64766,116.076185,120.050417,114.005056,116.176452,119.748132,0]),
array([119.560959]),
array([120.722814,121.715067,112.225388,119.587645,113.713141,115.907483,112.682689]),
array([104.517922]),
array([115.663892,0]),
array([63.587867,61.701763,65.828394,65.478799,64.329988,64.466234]),
array([102.26179,106.109686,104.240045,97.376677,0]),
array([61.970159,63.552435,63.527779]),
array([46.681639,0]),
array([68.319516,67.518211,64.12372,63.25991,62.247051,62.727053,64.330213,62.504148,62.60179,64.254876,63.016596,61.545422,70.428754,64.256579]),
array([68.35898,0]),
array([65.91767,0]),
array([99.276545,63.09418]),
array([110.757892]),
array([101.695412,97.3551,103.651849,98.674612,101.489697,95.776171,93.769111]),
array([115.426091,114.557813,112.378337,112.764815,112.829044,118.073596,121.854587,112.218252,0]),
array([76.050082,81.826641]),
array([55.837516,0]),
array([53.914846,0]),
array([57.142911,49.746124]),
array([81.985566,83.455212,0]),
array([19.07527,17.534056,18.308825,16.976196,26.518784,13.550386,27.169431]),
array([19.487683,0]),
array([69.593695,15.513378,0]),
array([22.419518,0]),
array([62.700929,65.857566,65.329896,62.630224,62.818629,63.327828,62.842696,61.780094,63.497474,63.263916,0]),
array([65.44411]),
array([39.945411,0]),
array([54.566159,52.351925,0]),
array([48.833634,51.96113,43.905454]),
array([45.115269,0]),
array([39.432184,41.609085]),
array([57.360859,56.829858,56.114498,37.584681,47.092803,45.183594,48.10099,40.64085,45.563823,46.362154,47.58339,42.036205,53.843129,48.615792,46.09461,41.091158]),
array([61.822224]),
array([64.985885]),
array([52.100338,53.901369,50.349714,46.423973,45.311941]),
array([51.329041,46.369408,48.49704,44.378687,39.091191,47.395876,43.128313,42.668541,39.590855,48.206744,39.967254,48.757788,53.17852,42.434501,53.970804,48.457022,41.694401,40.47495,38.409667,29.659278,36.113851,34.419992,35.961203,34.527886,36.091188,34.182482,36.621747,34.001548,36.701655,36.550368,35.453368,0]),
array([100.26413]),
array([59.803448,64.98659,61.295943,61.871645,0]),
array([57.902942,0]),
array([97.14213,97.061871,100.175351,98.057534,98.124923,98.582964,98.239735,95.374906,100.474827,97.994291,99.24519,90.281732,0]),
array([104.499553,99.893875,105.689028]),
array([118.093261,112.670618]),
array([56.690777,57.854865,104.631083,96.578657,19.369373,36.055511,0]),
array([120.681347,95.01842]),
array([119.084688]),
array([100.906025,0]),
array([53.708583,45.457006,47.150474]),
array([37.250974,0]),
array([58.009686,0]),
array([9.401849,25.005908,23.160453,18.424911,14.368311,34.55151,44.182814,46.982657,45.268428,35.706873,63.783778,61.839094,65.141288,8.364276,65.865737,64.529628,65.541939,62.001157,63.56765,63.670219,64.864772,65.37166,70.560186,18.784186,39.463636,54.503758]),
array([81.917648]),
array([63.990381,0]),
array([34.783975,0]),
array([37.239531,0]),
array([56.38689,55.86906,50.587579,47.205102,75.265398,89.123756,87.640988,92.634709,75.979384,34.404875]),
array([100.1699,0]),
array([59.390546]),
array([62.616987,0]),
array([78.964803,0]),
array([50.46603,49.556033,52.316229,51.705553]),
array([59.269219]),
array([30.57626,0]),
array([43.532937,37.577263,0]),
array([14.66571,12.545415]),
array([32.604615,0]),
array([38.013007,39.107991,45.680369,45.40546,46.919,42.620643,46.293366]),
array([10.981883,0]),
array([13.859956,16.826278,21.065107,0]),
array([58.457734,57.013053]),
array([67.151716,35.026834,0]),
array([102.12198,0]),
array([96.671271]),
array([95.43829]),
array([98.376269,0]),
array([115.136236,0]),
array([63.271778,60.25141,56.775802,64.031583,65.071863,63.884083,66.728555]),
array([68.571028,0]),
array([50.717305,62.271433,62.673545,0]),
array([25.128979]),
array([16.237864,12.30601]),
array([55.661086,0]),
array([57.45447]),
array([32.394911,0]),
array([96.299636]),
array([97.187937,98.175599,93.54352,98.265153,96.06457,98.796469,99.335219,90.857966,0]),
array([68.866312]),
array([15.576085,14.728396,12.102225,11.800411,13.597494,12.587789,12.64804,15.175884,11.733915,15.923911]),
array([54.3541,0]),
array([64.328944,63.682131]),
array([17.957743]),
array([26.658819]),
array([24.985272,11.761112]),
array([100.139396,94.401117,108.376413,100.256779,111.242512,104.622767,105.177336,99.523374,101.434924,103.250436,108.59888,96.319688,96.153146,100.393066,98.638852,111.178332,100.181513,106.172373,111.741047,94.606597,111.48,56.430606,22.436691,117.726398,16.126347,82.822131]),
array([30.83,32.925749,57.88726,70.415038]),
array([36.845711,64.863864]),
array([64.164515,65.347128,61.824958,62.431242,0]),
array([98.435825,26.649147,82.983968]),
array([23.68511,73.14854,69.000414]),
array([26.136316,0]),
array([74.676027]),
array([86.872395,87.108905,86.081124,88.275383,87.272314,88.321727,85.848906,83.955027,83.61735,85.256788,85.519034,87.582984,86.936415,86.994605,85.193634,84.558374,86.527961,86.339139,85.157765,0]),
array([26.010308,0]),
array([27.582865,56.243481,55.952741,57.545179,56.478581,58.57928,57.738439,32.676952,23.321863,63.598598,0]),
array([29.559244]),
array([13.270992,0]),
array([49.716172,47.010634,52.344326,0]),
array([58.635671,56.994754,51.437054,53.718634,74.377766,0]),
array([61.776538,65.968156,65.639422,58.46977,63.174812,64.264095,64.191451,0]),
array([61.81957]),
array([78.693385,0]),
array([76.086727,56.223075,0]),
array([65.213763,61.788592,61.886711,62.191633,63.814079,61.928682,65.628281,62.962418,62.00176,65.311245,65.493693,65.36205,63.641704,65.963658,64.232348,62.448794,0]),
array([40.364665,43.039606,0]),
array([3.187765,0]),
array([13.748557,0])
]

d=[data_1]
names=[ 'paleobioDB_angiosperms_1']
def get_data(i): return d[i]
def get_out_name(i): return  names[i]
taxa_names=['Acer_indivisum','Acer_newberryi','Acer_silberlingi','Ailanthipites_marginatus_cf.','Ailanthipites_mulleri','Ailanthipites_paenestriatus','Ailanthipites_sp.','Ailanthipites_sp._A_informal','Albertipollenites_?_perforatus','Albertipollenites_anguloluminosus','Araliaceoipollenites_?_sp.','Araliaceoipollenites_euphorii','Araliaceoipollenites_matanamadhensis_n._sp.','Araliaceoipollenites_potoniaei_n._sp.','Araliaceoipollenites_psilatus_n._sp.','Araliaceoipollenites_reticulatus_n._sp.','Araliaceoipollenites_reticuloides','Araliaceoipollenites_sp.','Baculamonocolpites_hammenii_n._sp.','Baculamonocolpites_sp.','Basopollis_obscurocostatus','Basopollis_sp.','Brevitricolpites_macroexinatus_n._sp.','Brevitricolpites_microechinatus_n._sp.','Brevitricolpites_sp.','Brevitricolpites_variabilis','Callistopollenites_radiostriatus','Callistopollenites_sp.','Caloda_n._gen._delevoryana_n._sp.','Carpestella_n._gen._lacunata_n._sp.','Catinipollis_geiseltalensis','Celastrophyllum_acutidens','Celastrophyllum_acutidens_?','Celastrophyllum_carolinensis','Celastrophyllum_cf._sp.','Celastrophyllum_crenatum','Celastrophyllum_elegans','Celastrophyllum_gyminaefolium','Celastrophyllum_perryi','Celastrophyllum_pulchrum_n._sp.','Celastrophyllum_subprotophyllum','Clavatricolpites_densiclavatus','Clavatricolpites_densiclavatus_n._sp.','Clavatricolpites_gracilis','Clavatricolpites_sp._1_informal','Colombipollis_tropicalis','Cupanoides_corrugatus_n._sp.','Cupanoides_depressus_n._sp.','Cupanoides_grandis','Cupanoides_grandis_n._sp.','Cupanoides_inflatus_n._sp.','Cupanoides_n._gen._lobatus_n._sp.','Cupanoides_peruvianus','Cupanoides_pygmaeus_n._sp.','Cupanoides_subangulatus_n._sp.','Cupanoides_tumidus','Cupanoides_tumidus_n._sp.','Curvimonocolpites_inornatus','Dicotetradites_clavatus','Dicotetradites_meridianus','Dicotetradites_reticulatus_n._sp.','Dicotetradites_spp.','Dicotylophyllum_aff._anomalum','Dicotylophyllum_aff._sp._3_informal','Dicotylophyllum_agashi_n._sp.','Dicotylophyllum_aliquantuliserratum_n._sp.','Dicotylophyllum_angularis_n._sp.','Dicotylophyllum_anomalum','Dicotylophyllum_chitalii_n._sp.','Dicotylophyllum_cordatum_n._sp.','Dicotylophyllum_corifolium','Dicotylophyllum_dioscoreoides_n._sp.','Dicotylophyllum_elegans','Dicotylophyllum_expansolobum_n._sp.','Dicotylophyllum_hamameloides_n._sp.','Dicotylophyllum_hebronensis_n._sp.','Dicotylophyllum_kummerensis','Dicotylophyllum_lobatus_n._sp.','Dicotylophyllum_mercerensis','Dicotylophyllum_mercerensis_n._sp.','Dicotylophyllum_myrtophylloides_n._sp.','Dicotylophyllum_oblongatum_n._sp.','Dicotylophyllum_ovatodecurrens_n._sp.','Dicotylophyllum_panandhroensis_n._sp.','Dicotylophyllum_patilii_n._sp.','Dicotylophyllum_pinnatifidum_n._sp.','Dicotylophyllum_quadrinervatum_n._sp.','Dicotylophyllum_rosafluviatilis_n._sp.','Dicotylophyllum_sp.','Dicotylophyllum_sp._1','Dicotylophyllum_sp._1_informal','Dicotylophyllum_sp._10_informal','Dicotylophyllum_sp._11_informal','Dicotylophyllum_sp._12_informal','Dicotylophyllum_sp._13_informal','Dicotylophyllum_sp._14_informal','Dicotylophyllum_sp._15_informal','Dicotylophyllum_sp._16_informal','Dicotylophyllum_sp._17_informal','Dicotylophyllum_sp._2_informal','Dicotylophyllum_sp._3_informal','Dicotylophyllum_sp._4_informal','Dicotylophyllum_sp._5_informal','Dicotylophyllum_sp._6_informal','Dicotylophyllum_sp._7_informal','Dicotylophyllum_sp._8_informal','Dicotylophyllum_sp._9_informal','Dicotylophyllum_sp._A','Dicotylophyllum_sp._B','Dicotylophyllum_sp._informal','Dicotylophyllum_sp.1_informal','Dicotylophyllum_spp.','Dryoxylon_intertrappea_n._sp.','Dryoxylon_mahurzarii_n._sp.','Dryoxylon_mohgaoense_n._sp.','Dryoxylon_nahanai_n._sp.','Dryoxylon_simarubaceus_n._sp.','Dryoxylon_sp.','Dryoxylon_spp.','Dryoxylon_tiliaceoides_n._sp.','Duplotriporites_ariani','Echimonocolpites_densus','Echimonocolpites_sp.','Echimonocolpites_sp._informal','Echitetracolpites_?_sp.','Echitetracolpites_?_tennuiexinatus','Echitetracolpites_?_tennuiexinatus_n._sp.','Echitetracolpites_?_tenuiexinatus_n._sp.','Favitricolporites_australis','Favitricolporites_australis_n._sp.','Favitricolporites_baculoferous','Favitricolporites_complex','Favitricolporites_complex_n._sp.','Favitricolporites_medireticulatus','Favitricolporites_medireticulatus_n._sp.','Forcipites_longus','Forcipites_longus_cf.','Forcipites_sabulosus','Forcipites_sp._A_informal','Forcipites_stipulatus_cf.','Foveotricolporites_caldensis','Foveotricolporites_crassiexinus_cf.','Foveotricolporites_fossulatus_n._sp.','Foveotricolporites_marginatus','Foveotricolporites_porololongatus','Foveotricolporites_rugulatus','Foveotricolporites_rugulatus_n._sp.','Foveotricolporites_sp.','Foveotricolporites_spp._informal','Foveotricolporites_voluminosus','Foveotriporites_hammenii','Foveotriporites_sp.','Fremontia_lobata_n._sp.','Gambierina_(Gambierina_cf.)_edwardsii','Gambierina_edwardsii','Gambierina_edwardsii_cf.','Gambierina_rudata','Gambierina_sp.','Gemmamonocolpites_amicus','Gemmamonocolpites_barbatus','Gemmamonocolpites_gemmatus','Gemmamonocolpites_gemmatus_n._sp.','Gemmamonocolpites_macrogemmatus','Gemmamonocolpites_ovatus','Gemmamonocolpites_perfectus_n._sp.','Gemmamonocolpites_pilulus','Gemmamonocolpites_sp.','Gemmatricolpites_pergemmatus','Gemmatricolpites_pulcher','Gemmatricolpites_sp.','Gemmatricolpites_sp._1_informal','Gemmatricolpites_sp._informal','Gemmatricolpites_subsphaericus','Gemmatricolpites_subsphaericus_cf.','Gemmatricolpites_subsphaericus_n._sp.','Glochidioxylon_sahnii_n._sp.','Glochidioxylon_tertiarum_n._sp.','Haloragacidites_amolosus','Haloragacidites_cf._harrisii','Haloragacidites_haloragoides','Haloragacidites_harrisii','Haloragacidites_myriophylloides','Haloragacidites_neyvelii_n._sp.','Haloragacidites_sp.','Haloragacidites_trioratus','Horniella_sp.','Icacinoxylon_pittiense','Icacinoxylon_pittiense_n._sp.','Jandufouria_minor_n._sp.','Kachaikenia_n._gen._compuesta_n._sp.','Kenella_aff._sp.','Kenella_filatovii','Ladakhipollenites_rubinii','Ladakhipollenites_simplex','Ladakhipollenites_sp.','Lefipania_padillae','Magnoliopsida_"Banara"_prehernandiensis_informal','Magnoliopsida_"Cassia"_argentinensis_informal','Magnoliopsida_"Celtis"_ameghenoi_Berry_informal','Magnoliopsida_"Celtis"_ameghenoi_informal','Magnoliopsida_"Coprosma"_incerta_informal','Magnoliopsida_"Cupania"_grosseserrata_informal','Magnoliopsida_"Cupania"_latifolioides_informal','Magnoliopsida_"Myrica"_mira_informal','Magnoliopsida_"Schmidelia"_proedulis_informal','Magnoliopsida_"Sterculia"_patagonica_informal','Magnoliopsida_"Tetracera"_patagonica_informal','Magnoliopsida_cf._"Banara"_prehernandiensis_informal','Magnoliopsida_cf._Allophylus_eduliformis_informal','Magnoliopsida_cf._Banara_prehernandiensis_informal','Magnoliopsida_cf._Cochlospermum_previtifolium_informal','Magnoliopsida_cf._Macaranga_or_Mallotus_informal','Magnoliopsida_morphotype_a_informal','Magnoliopsida_morphotype_b_informal','Malvacipollis_argenlina','Malvacipollis_argentina','Malvacipollis_argentina_n._sp.','Malvacipollis_comodoroensis','Malvacipollis_diversus','Malvacipollis_diversus_?','Malvacipollis_diversus_cf.','Malvacipollis_duratus','Malvacipollis_gracilis_cf.','Malvacipollis_sp.','Malvacipollis_sp._1_informal','Malvacipollis_sp._2_informal','Malvacipollis_sp._3_informal','Malvacipollis_sp._informal','Malvacipollis_spinyspora','Malvacipollis_spp.','Malvacipollis_subtilis','Metcalfeoxylon_indet.','Metcalfeoxylon_kirtlandense','Octacolpites_brevicolpa_n._sp.','Pandaniidites_indet.','Pandaniidites_radicus','Pandaniidites_sp.','Paraphyllanthoxylon_alabamense_cf.','Paraphyllanthoxylon_anasazi','Paraphyllanthoxylon_arizonense','Paraphyllanthoxylon_gaulkheraense_n._sp.','Paraphyllanthoxylon_idahoense','Paraphyllanthoxylon_keriense_n._sp.','Paraphyllanthoxylon_mohgaoensis_n._sp.','Paraphyllanthoxylon_palaeoemblica_n._sp.','Paraphyllanthoxylon_utahense','Parsonsidites_?_sp.','Parsonsidites_multiporus','Parsonsidites_sp.','Periporopollenites_demarcatus','Periporopollenites_pallidus','Periporopollenites_polyoratus','Periporopollenites_polyoratus_cf.','Periporopollenites_polyratus','Periporopollenites_sp._A_informal','Periporopollenites_spinosus','Periporopollenites_spp.','Periporopollenites_versicus','Periporopollenites_vesicus','Periporopollenites_vesicus_cf.','Periretisyncolpites_giganteus','Periretisyncolpites_phosphaticus','Periretisyncolpites_sp.','Phyllanthinium_bangalamodense_n._sp.','Poloretitricolpites_n._gen._absolutus_n._sp.','Polycolpites_cooksonii_n._sp.','Polycolpites_ghoshii_n._sp.','Polycolpites_langstonii','Polycolpites_pococki_n._sp.','Polycolpites_sp.','Polycolpites_speciosus_n._sp.','Polycolpites_vimalii','Polycolpites_vimalii_n._sp.','Polyporina_bipatterna','Polyporina_chenopodiaceoides','Polyporina_excellens','Polyporina_globosa','Polyporina_mediporus_n._sp.','Polyporina_romeroi','Polyporina_romeroi_n._sp.','Psilatricolporites_cassioides_n._sp.','Psilatricolporites_crassus','Psilatricolporites_cyamus','Psilatricolporites_distinctus','Psilatricolporites_innovatus_informal','Psilatricolporites_minor_n._sp.','Psilatricolporites_normalis','Psilatricolporites_obscurus','Psilatricolporites_optimus','Psilatricolporites_pseudomarginatus','Psilatricolporites_salamanquensis','Psilatricolporites_salamanquensis_cf.','Psilatricolporites_sp.','Psilatricolporites_sp._1_informal','Psilatricolporites_sp._2_informal','Psilatricolporites_sp._cf._salamanquensis_informal','Psilatricolporites_sp._cf._salamanquesis_informal','Psilatricolporites_sp._informal','Psilatricolporites_sp.1_informal','Psilatricolporites_spp.','Psilatricolporites_vanus','Psilatricolporites_venezuelanus','Quadraplanus_brossus','Quereuxia_angulata','Quereuxia_sp.','Retitricolpites_florentinus','Retitricolpites_perforatus','Retitricolporites_annaeoides','Retitricolporites_chubutensis','Retitricolporites_chubutensis_cf.','Retitricolporites_chubutensis_n._sp.','Retitricolporites_colpotransversalis','Retitricolporites_craceus','Retitricolporites_cuddalorense_n._sp.','Retitricolporites_dissimulatus_informal','Retitricolporites_equatorialis','Retitricolporites_finitus','Retitricolporites_ghoshii_n._sp.','Retitricolporites_hispidus','Retitricolporites_marianis','Retitricolporites_medius','Retitricolporites_microreticulatus','Retitricolporites_minutiformis','Retitricolporites_minutiformis_cf.','Retitricolporites_porolalongatus','Retitricolporites_profundus','Retitricolporites_pseudorhoipites','Retitricolporites_saskiae','Retitricolporites_sp.','Retitricolporites_sp._1_informal','Retitricolporites_sp._informal','Retitricolporites_spp.','Rhoipites_?_sp.','Rhoipites_"_cienaguensis','Rhoipites_alveolatus','Rhoipites_angustus','Rhoipites_aralioides','Rhoipites_baculatus','Rhoipites_baculatus_n._sp.','Rhoipites_bradleyi','Rhoipites_cienagensis','Rhoipites_cienagensis_n._sp.','Rhoipites_cienaguensis','Rhoipites_cissus','Rhoipites_exiguus','Rhoipites_guianensis','Rhoipites_hispidus','Rhoipites_isoreticulatus_?','Rhoipites_karamuensis','Rhoipites_kutchensis_n._sp.','Rhoipites_microreticulatus_cf.','Rhoipites_minusculus','Rhoipites_minusculus_n._sp.','Rhoipites_prolatus','Rhoipites_romeroi','Rhoipites_santafesii','Rhoipites_sp.','Rhoipites_sp._1_informal','Rhoipites_sp._A_informal','Rhoipites_sp._B_informal','Rhoipites_sp._C_informal','Rhoipites_sp._D_informal','Rhoipites_sp._E_informal','Rhoipites_sphaerica','Rhoipites_spp.','Rhoipites_striatus_n._sp.','Rousea_?_georgensis','Rousea_delicata_cf.','Rousea_delicipollis','Rousea_doylei','Rousea_georgensis','Rousea_georgensis_cf.','Rousea_geraniodes','Rousea_geranioides_aff.','Rousea_geranoides','Rousea_marthae_cf.','Rousea_microreticulata','Rousea_miculipollis_cf.','Rousea_minuscula','Rousea_monilifera','Rousea_patagonica','Rousea_patagonica_cf.','Rousea_patagonica_n._sp.','Rousea_sp.','Rousea_sp._informal','Rousea_spp.','Rousea_spp._informal','Scabrastephanocolpites_lepidus','Scabrastephanocolpites_lisamae','Scabrastephanocolpites_sp.','Scabrastephanocolpites_vanegensis','Senipites_drumhellerensis','Senipites_patagonica','Senipites_patogonica','Senipites_sp.','Senipites_sp.1_informal','Senipites_tercrassata','Senipites_tercrassata_n._sp.','Siltaria_abouziarovae','Siltaria_mariposa','Siltaria_media','Siltaria_medius','Siltaria_pacata_group_informal','Siltaria_sp.','Srivastavapollenites_exoticus','Srivastavapollenites_n._gen._exoticus_n._sp.','Striatopollis_?_tennuistriatus_n._sp.','Striatopollis_catatumbus','Striatopollis_dubius','Striatopollis_exiguus','Striatopollis_minor','Striatopollis_paraneus','Striatopollis_paraneus_cf.','Striatopollis_reticulatus','Striatopollis_sp.','Striatopollis_spp._informal','Striatopollis_trochuensis','Striatopollis_vermimurus','Striatricolporites_agustinus','Striatricolporites_digitatus','Striatricolporites_digitatus_n._sp.','Striatricolporites_gamerroi','Striatricolporites_gamerroi_cf.','Striatricolporites_gamerroi_n._sp.','Striatricolporites_porolalongatus','Striatricolporites_reticulatus','Striatricolporites_sp.','Striatricolporites_spp._informal','Syndemicolpites_petriellai','Syndemicolpites_petriellai_n._sp.','Syndemicolpites_typicus','Tetracolporites_?_oamaruensis','Tetracolporites_brevicolpus_n._sp.','Tetracolporites_longicolpus_n._sp.','Tetracolporites_pachyexinatus_n._sp.','Tetracolporites_palynius','Tetracolporites_paucus_n._sp.','Tetracolporites_psilatus_n._sp.','Tetracolporites_sp.','Tetracolporites_spectabilis','Tetracolporites_spp.','Tetracolporites_verrucosus','Thorhallenia_dentata','Thorhallenia_form_A_informal','Thorhallenia_form_B_informal','Thorhallenia_n._gen._dentata_n._sp.','Thorphyllum_n._gen._patagonica_n._sp.','Trapago_angulata','Trapago_angulate','Tricolpites_anguloluminosus','Tricolporites_adelaidenis','Tricolporites_adelaidensis','Tricolporites_adelaidensis_cf.','Tricolporites_ambiguus','Tricolporites_angurium_cf.','Tricolporites_cf._lillei','Tricolporites_distinctus','Tricolporites_lillei','Tricolporites_microreticulatus','Tricolporites_moultonii','Tricolporites_pachyexinus','Tricolporites_paenestriatus','Tricolporites_retequetrus','Tricolporites_scabratus','Tricolporites_sp.','Tricolporites_sp._1_informal','Tricolporites_sp._2_informal','Tricolporites_sp._3_informal','Tricolporites_sp._A_informal','Tricolporites_sp._B_informal','Tricolporites_sp._C_informal','Tricolporites_sp._D_informal','Tricolporites_sp.1_informal','Tricolporites_sphaerica','Tricolporites_spp.','Tricolporites_spp._indet._informal','Tricolporites_trioblatus_cf.','Tricolporites_valvatus','Ulmoideipites_krempii','Ulmoideipites_patagonicus','Ulmoideipites_patagonicus_n._sp.','Ulmoideipites_planaeriformis','Ulmoideipites_tricostatus','Verrustephanoporites_simplex','Verrustephanoporites_sp.','Xanthophyllum_cuddalorense_n._sp.','Xanthophyllum_mioglaucum_n._sp.']
def get_taxa_names(): return taxa_names
