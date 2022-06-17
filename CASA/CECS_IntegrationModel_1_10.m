%CECS_IntegrationModel_1.m
%There are setups plus 3 blocks

function CECS_IntegrationModel_1(ARD2run)  %Unremark if calling from SLURM
%clear all

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%Setups
% global Disturbancekills outdirectoryroot ARDTile ii years
% global Disturbfractremoval Freshslowdown

Blocks2run = [3];

Tiles2run = ['h01v05'; 'h01v06'; 'h01v07'; 'h01v08'; 'h01v09'; 'h01v10'; 'h02v05'; 'h02v06'; 'h02v07';
    'h02v08'; 'h02v09'; 'h02v10'; 'h02v11'; 'h02v12'; 'h03v06'; 'h03v07'; 'h03v08'; 'h03v09'; 'h03v10';
    'h03v11'; 'h03v12'; 'h03v13'; 'h04v06'; 'h04v09'; 'h04v10'; 'h04v11'; 'h04v12'; 'h04v13'; 'h05v10';
    'h05v11'; 'h05v12'; 'h05v13'; 'h06v12'];

ARD2run = str2double(ARD2run); %Unremark if calling from SLURM
%ARD2run = 27; %Unremark and specify if running just one tile - or remark if running in SLURM

SubTiles = ''; %Unremark if running full ARDTile
%SubTiles = '/SubTiles'; %Unremark if running for individual Subtiles

SubTile2run = ''; %Unremark if running full ARDTile
%SubTile2run = '_SubTile0705'; %Unremark and specify if running for individual Subtile - or remark if running full ARDTile

disp(ARD2run);
ARDTile = Tiles2run(ARD2run,:);
disp(ARDTile);
%
% indirectoryroot = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Biomass']; %New path
% outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Biomass']; %New path
% mkdir(outdirectoryroot);
% copyfile(indirectoryroot, outdirectoryroot);
%
% indirectoryroot = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Elevation']; %New path
% outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Elevation']; %New path
% mkdir(outdirectoryroot);
% copyfile(indirectoryroot, outdirectoryroot);
%
% indirectoryroot = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Meteorology']; %New path
% outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Meteorology']; %New path
% mkdir(outdirectoryroot);
% copyfile(indirectoryroot, outdirectoryroot);
%
% indirectoryroot = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/DrySeasonDrawdown']; %New path
% outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/DrySeasonDrawdown']; %New path
% mkdir(outdirectoryroot);
% copyfile(indirectoryroot, outdirectoryroot);
%
% indirectoryroot = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/Landsatstacker2/Mosaiced']; %New path
% outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/Landsatstacker2/Mosaiced']; %New path
% mkdir(outdirectoryroot);
% copyfile(indirectoryroot, outdirectoryroot);
%
% indirectoryroot = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/SolarTopography']; %New path
% outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/SolarTopography']; %New path
% mkdir(outdirectoryroot);
% copyfile(indirectoryroot, outdirectoryroot);

% for ii = 2:33;
% ARDTile = Tiles2run(ii,:)
% % wrongoutdirectoryroot = ['/dfs5/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile  '/RawInputs/Block1']; %New path
% % outdirectoryroot = ['/dfs5/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/IntegrationModel']; %New path
% % mkdir(outdirectoryroot);
% % movefile(wrongoutdirectoryroot, outdirectoryroot);
% wrongoutdirectoryroot = ['/dfs5/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile  '/RawInputs']; %New path
% rmdir(wrongoutdirectoryroot, 's');
% end

indirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile]; %New path
outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile]; %New path

outname = [outdirectoryroot '/IntegrationModel/Block1']
%rmdir(outname, 's');
mkdir(outname);

outname = [outdirectoryroot '/IntegrationModel/Block2']
%rmdir(outname, 's');
mkdir(outname);

outname = [outdirectoryroot '/IntegrationModel/Block3']
%rmdir(outname, 's');
mkdir(outname);

Beginyear = 1985;
Endyear = 2021;
%Endyear = 1986;

years = Beginyear:Endyear;

mnth = [];
for jj = 1:12
    newmnth = num2str(jj);
    if jj < 10
        newmnth = [num2str(0) newmnth];
    end
    mnth = [mnth; newmnth];
end

startii = 1; %This lets you start later in the series - default is 1 for begining of Landsat


%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%Block 1: Gas Exchange and water balance
% Regressions based on CA flux tower observations, analysed Sept 2020

%Layers needed
% NIRv monthly average continuous, calculated by CECS_Landsatstacker_Step2_2
% NDVI monthly average calculated by CECS_Landsatstacker_ Step2_2
% Temp monthly average from PRISM, prepared by CECS_TileMaker
% Precip monthly average from PRISM, prepared by CECS_TileMaker
% Light monthly sky transmittance from PRISM, prepared by CECS_TileMaker
% Light monthly clear sky average from based on DEM calculation in CECS_SolarTopography_1
% DSD Dry Season Drawdown not time varying, calculated by CECS_DrySeasonDrawdown_1
% Previous months Soilmoisture - calcuated here

%Read in layers that are static
Inname = [indirectoryroot '/DrySeasonDrawdown/' ARDTile '_MaxDSD.mat']; ; %New path
load(Inname); % Maxbucketsize
BucketMultiplier = 2; %Multiply by DSD to calculate how much water perenial veg can access - base on Klos paper

%Solar radition based on topography
Inname = [indirectoryroot '/SolarTopography/' 'SolarRadiation' ARDTile '.mat']; %New path
load(Inname); %'srad' This prepared with SolarTopography - sloped TOA
Solarstack = []; %And this is the PRISM_soltrans_30yr_normal via CECS_TileMaker
for jj = 1:12
    InnameSolar = [indirectoryroot '/RAWInputs/Meteorology/PRISMSolar/PRISMmonthlysolar_' num2str(mnth(jj,:)) '.mat'];
    load(InnameSolar); %layer
    Solarstack = cat(3, Solarstack, layer);
end
srad = Solarstack .* srad ./24; %And put them together - sloped TOA*PRISM_soltrans - note, this may not be as great as it sounds - diffs in dir vs diffuse - but not terrible

%Main block of code
%Loop through the years
if logical(sum(Blocks2run == 1))
    for ii = startii:size(years,2); %Annual looper
        %for ii = 1:1; %Annual looper for just a few years

        %Read in 36 months of data
        Innametype = ['_NIRv_Meanbymonth_'];
        if ii > 1;
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str((years(ii)-1)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NIRv = double(Fullstack);
        end

        if ii == 1;
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NIRv = double(Fullstack);
        end

        Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii)) '.mat']  %New path
        load(Inname); %,'Fullstack'
        NIRv = cat(3,NIRv,double(Fullstack));

        if ii == size(years,2);
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NIRv = cat(3,NIRv,double(Fullstack));
        end

        if ii < size(years,2);
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii+1)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NIRv = cat(3,NIRv,double(Fullstack));
        end

        NIRv = NIRv./10000;
        NIRv = NIRv(:,:,7:30); %Trim to cernter 24 months (Jul-June)

        Innametype = ['_NDVI_Meanbymonth_'];
        if ii > 1;
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str((years(ii)-1)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NDVI = double(Fullstack);
        end

        if ii == 1;
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NDVI = double(Fullstack);
        end

        Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii)) '.mat']  %New path
        load(Inname); %,'Fullstack'
        NDVI = cat(3,NDVI,double(Fullstack));

        if ii == size(years,2);
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NDVI = cat(3,NDVI,double(Fullstack));
        end

        if ii < size(years,2);
            Inname = [indirectoryroot '/Landsatstacker2/Mosaiced/' ARDTile Innametype num2str(years(ii+1)) '.mat']  %New path
            load(Inname); %,'Fullstack'
            NDVI = cat(3,NDVI,double(Fullstack));
        end

        NDVI = NDVI./10000;
        NDVI = NDVI(:,:,7:30); %Trim to 24 months
        NDVI = single(NDVI(:,:,7:18));

        Precipstack = [];
        for jj = 1:12
            %if ~isempty(SubTile2run);
            InnamePrecip = [indirectoryroot '/RAWInputs/Meteorology/PRISMPrecip/PRISM_Precip_' num2str(years(ii)) mnth(jj,:) '.mat'];
            load(InnamePrecip);
            Precipstack = cat(3, Precipstack, layer);
            %             else
            %                 InnamePrecip = [indirectoryroot '/RAWInputs/Meteorology/PRISMPrecip/PRISM_Precip_' num2str(years(ii)) mnth(jj,:) '.tif'];
            %                 [newP] = geotiffread(InnamePrecip);
            %                 Precipstack = cat(3, Precipstack, layer);
            %             end
        end;

        Tempstack = [];
        for jj = 1:12
            %             if ~isempty(SubTile2run);
            InnameTemp = [indirectoryroot '/RAWInputs/Meteorology/PRISMTemp/PRISM_Tmean_' num2str(years(ii)) mnth(jj,:) '.mat'];
            load(InnameTemp); %layer
            Tempstack = cat(3, Tempstack, layer);
            %             else
            %                 InnameTemp = [indirectoryroot '/RAWInputs/Meteorology/PRISMTemp/PRISM_Tmean_' num2str(years(ii)) mnth(jj,:) '.tif'];
            %         load(InnameTemp); %layer
            %         Tempstack = cat(3, Tempstack, layer);
            %             end
        end;

        for jj = 1:12
            jj
            days = datenum(years(ii),jj+1,1) - datenum(years(ii),jj,1);
            workingNIRv = NIRv(:,:,jj);
            workingNDVI = NDVI(:,:,jj);
            workingP = Precipstack(:,:,jj);
            workingT = Tempstack(:,:,jj);
            workingsrad = srad(:,:,jj);
            Soilbucketsize = BucketMultiplier.*Maxbucketsize; % Soilbucketsize = 2*Dry Season Drawdown

            if ii ==1 & jj == 1;;
                Soilmoisture = Soilbucketsize;
            end

            %GPP calculation
            GPPmax = 35.864*workingNIRv - 0.5412; %where NIRv an input from Landsat and GPPmax is gC/m2day
            GPPTlimit = 0.000121*workingT.^3 - 0.007162*workingT.^2 + 0.114333*workingT + 0.272396; %where Temper from PRISM
            TooCold = GPPTlimit <=0;
            GPPTlimit(TooCold) = 0;
            GPPLightlimit = 0.001404*workingsrad + 0.358605; %Where Light topographic calculation and solar angle
            GPPWaterlimit = 0.001746*Soilmoisture + 0.449765; % Where soilmoisture from water balance calculation below during previous month
            TooWet = GPPWaterlimit >= 1;
            GPPWaterlimit(TooWet) = 1;
            GPPpredicted = GPPmax .* GPPTlimit .* GPPLightlimit;  %Main calculation
            GPPpredicted = 1.9341.*GPPpredicted; %Correction back to match observations
            GPPpredicted = days.*GPPpredicted; %Convert to monthly total

            %Next AET calculation
            AETmax = 5.1516.*workingNDVI.^1.3037; %where NDVI an input from Landsat
            AETmax = single(real(AETmax));
            AETWaterlimit = 0.000174.*Soilmoisture + 0.696063; %where Soilmoisture from previous month
            AETTlimit = 0.0131.*workingT + 0.5483; %where Temp from PRISM
            AETLightlimit = 0.002636*workingsrad + 0.122826; %
            AETpredicted = AETmax .* AETLightlimit .* AETWaterlimit .* AETTlimit; %Main calculation
            AETpredicted = AETpredicted .* 1.7029;  %Correction back to match observations
            AETpredicted = days.*AETpredicted; %Convert to monthly total mm/month

            %And now water balance calculation
            %This partions water into 5 fluxes - Precip, predicted ET,
            %curtailed ET, delta soil, runoff, based on a single 2*DSD based bucket
            Soilmoisture = Soilmoisture + workingP - AETpredicted; %where Soilmoisture enters as the soil moisture during the previous month
            Desiccated = Soilmoisture;
            Desiccated(Soilmoisture >= 0) = 0; %Desiccated is the amount of ET that is prevented becuase no water to evap
            Soilmoisture(Soilmoisture < 0) = 0; %So soil moisture can't actually go below 0
            Runoff = Soilmoisture - Soilbucketsize;
            Runoff(Runoff<0) = 0; %So no runoff unless bucket overflows
            Soilmoisture = Soilmoisture - Runoff;

            %Writeouts - monthly - GPPpredicted, AETpredicted, Soilmoisture, Runoff, Desiccated
            Outname = [outdirectoryroot '/IntegrationModel/Block1/' ARDTile '_GPPpredicted_' num2str(years(ii)) mnth(jj,:) '.mat']   %New path
            save(Outname,'GPPpredicted','-v7.3')
            Outname = [outdirectoryroot  '/IntegrationModel/Block1/' ARDTile '_AETpredicted_' num2str(years(ii)) mnth(jj,:) '.mat']   %New path
            save(Outname,'AETpredicted','-v7.3')
            Outname = [outdirectoryroot  '/IntegrationModel/Block1/' ARDTile '_Soilmoisture_' num2str(years(ii)) mnth(jj,:) '.mat']   %New path
            save(Outname,'Soilmoisture','-v7.3')
            Outname = [outdirectoryroot  '/IntegrationModel/Block1/' ARDTile '_Runoff_' num2str(years(ii)) mnth(jj,:) '.mat']   %New path
            save(Outname,'Runoff','-v7.3')
            Outname = [outdirectoryroot  '/IntegrationModel/Block1/' ARDTile '_Desiccated_' num2str(years(ii)) mnth(jj,:) '.mat']   %New path
            save(Outname,'Desiccated','-v7.3')
        end
    end
    clearvars -except BucketMultiplier Blocks2run SubTile2run SubTile InnameVeg maxcorrect startii ii Soilmoisture ARDTile ARDTile indirectoryroot outdirectoryroot Beginyear Endyear years Years2run Months2run mnth  Maxbucketsize srad Correction
end

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%Block 2: Plant growth and death
% Allocation, tissue growth and turnover, mortality
%
% Lots of ideas from Python version of CASA GFED 4 from JTR
% And lots of new ideas

clearvars -except  Blocks2run SubTile2run SubTile InnameVeg startii ARDTile indirectoryroot outdirectoryroot Beginyear Endyear years Years2run Months2run mnth

%Setups
%Some inputs and constants
Fherbleaf = 0.5; %Fractional allocation - this one is leaf/total - CASA assume leaf = root for herbaceous
Fherbroot = 0.5; %Roots/total
Fevergreenfroot = 0.5; %This one is fineroot/leaves - total conjecture
Fevergreenwoodyroot = 0.5; %This one is woody root/wood- total conjecture

%Growth effecciency/allocation to respiration/NPP to GPP
Growthefftree = 0.5; %Waring/Ryan type analyses
Growtheffshrub = 0.5;
Growtheffherb = 0.5;

%Specific Leaf Weights
SLeafwtevrg = 150; %150 g/m2 - gotta start somewhere
SLeafwtherb = 75; %75 g/m2

%Carbon content
Ccontent = 0.5; %gC/biomass

%Specify lifespans for evergreen leafs and various diameter stems
Tleafevrg = 3; %Table 4.20.6 in FVS FFE
Twood1hr = 3.69; %Wood turnovers are based on Enquist type analyses
Twood10hr = 6.80;%
Twood100hr = 14.78;%
Twood1000hr = 29.00;%
Twoodtrunk = 107.72;%

Allocate1hr = 5.91; %g/m2, this is mass of 1 hr branch needed to support evergreen leaf mass, source as above
Allocate10hr = 34.51; %g/m2, needed for 10 hr, assumes Leandro branching rule and power rule based branch length
Allocate100hr = 123.50; %Various Enquist type analyses 
Allocate1000hr = 374.26; %
Allocatetrunk = 1185.49; %

%Kcorrection = 0.724; %based on new, weighted NPP partition - 7/14/2021
Kcorrection = 0.536; %this is median based on weighted NPP partition - 5/29/2022 
%There are lots of ways to calculate this - (mean gives 0.5035 - other approaches give .5 to .56) 

%Disturbance Kills
%3 column x 6 row table of fraction live pools lost in disturbance,
%where columns are disturbance type and rows are pool - these are scalers
%that get multiplied by the canopy loss - a value of 0 means no death ever
%regardless of canopy loss, a value of 1 means death for that pool is
% = canopy loss, and a value of 2 means greater death = 2 * canopy loss
%Columns are fire, management, dieoff
%Rows are: Row 1 Wood1hr, Row 2 Wood10hr,Row 3 Wood100hr, Row 4 Wood1000hr,
%Row 5 Woodtrunk,Row 6 Woodroot, Row7 Evrgleaf
Disturbancekills = [1 1 1; 1 1 1; 1 1 1; 1 1 1; 1 1 1; 1 1 1; 1 1 1];

%%%%%%%%%%
%This operates on 15 month blocks of Oct of previous year to Dec of current year
%This has a couple of advantages - it allows either calander or water year,
%and it reduces averaging and differencing step changes

if logical(sum(Blocks2run == 2))
    %First half of Block 2 - NPP
    if 1 > 2 %Use this to skip NPP calc if it was already done
        for ii = startii:size(years,2); %Annual looper
            %for ii = 37; %Annual looper
            %Calculate Ks
            Kleafevrgyear = 1/Tleafevrg; %Annual K
            Kleafevrgmonth = 1-(power(exp(-(1/Tleafevrg)), 1.0/12)); %Monthly K

            %Read in distubance and veg layers - prepared by VegCorrector
            Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_ShrubFracabsstack.mat']
            load(Inname); %'ShrubFracabsstack'
            ShrubFracabs = ShrubFracabsstack(:,:, ii);

            clear preShrubstack deltaFracShrubstack ShrubFracabsstack

            Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_TreeFracabsstack.mat']
            load(Inname); %'TreeFracabsstack'
            TreeFracabs = TreeFracabsstack(:,:, ii);

            Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_HerbFracabsstack.mat']
            load(Inname); %'HerbFracabsstack'
            HerbFracabs = HerbFracabsstack(:,:, ii);
            clear HerbFracabsstack

            Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_BareFracabsstack.mat']
            load(Inname); %'BareFracabsstack'
            BareFracabs = BareFracabsstack(:,:, ii);
            clear BareFracabsstack

            ShrubFracabs(ShrubFracabs < 0 | isnan(ShrubFracabs)) = 0;
            TreeFracabs(TreeFracabs < 0 | isnan(TreeFracabs)) = 0;
            HerbFracabs(HerbFracabs < 0 | isnan(HerbFracabs)) = 0;
            BareFracabs(BareFracabs < 0 | isnan(BareFracabs)) = 0;

            Livecover = HerbFracabs + ShrubFracabs + TreeFracabs;
            Totalcover = HerbFracabs + ShrubFracabs + TreeFracabs + BareFracabs;
            LAItotal = (Livecover)./(Totalcover); %Estimate LAIs from Jon's cover data
            LAItotal(LAItotal > 0.95) = 0.95;
            LAItotal = real(-2*log(1-LAItotal)); %For k = 0.5, adjust as needed

            HerbFrac = HerbFracabs./(Livecover);
            ShrubFrac  = ShrubFracabs./(Livecover);
            TreeFrac  = TreeFracabs./(Livecover);

            LAIherb = HerbFrac .* LAItotal;
            LAIshrub = ShrubFrac .* LAItotal;
            LAItree = TreeFrac .* LAItotal;

            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_LAIshrub_' num2str(years(ii)) '.mat']
            save(Outname,'LAIshrub','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_LAItree_' num2str(years(ii)) '.mat']
            save(Outname,'LAItree','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_LAIherb_' num2str(years(ii)) '.mat']
            save(Outname,'LAIherb','-v7.3')

            % Read in GPP monthly average from Block 1
            GPPstack = [];
            if ii == 1;
                for jj = 10:12
                    Inname = [outdirectoryroot '/IntegrationModel/Block1/' ARDTile '_GPPpredicted_' num2str(years(ii)) mnth(jj,:) '.mat']  %New path
                    load(Inname); %,'GPPpredicted'
                    GPPstack = cat(3, GPPstack, GPPpredicted);
                end;
            end;

            if ii > 1;
                for jj = 10:12
                    Inname = [outdirectoryroot '/IntegrationModel/Block1/' ARDTile '_GPPpredicted_' num2str(years(ii-1)) mnth(jj,:) '.mat']  %New path
                    load(Inname); %,'GPPpredicted'
                    GPPstack = cat(3, GPPstack, GPPpredicted);
                end;
            end;

            for jj = 1:12
                Inname = [outdirectoryroot '/IntegrationModel/Block1/' ARDTile '_GPPpredicted_' num2str(years(ii)) mnth(jj,:) '.mat']  %New path
                load(Inname); %,'GPPpredicted'
                GPPstack = cat(3, GPPstack, GPPpredicted);
            end;

            %Turn GPP into NPP - a critical step
            %GPP weighting coefs from TuningCoefs runs
            %Trees     0.22005 - so trees have more GPP/fractional cover than shrubs or herbs
            %Shrubs  0.10093 - this makes sense - they have longer growing season
            %Herbs  0.096378
            TreeFracGPP = TreeFrac .* 0.22005;
            ShrubFracGPP = ShrubFrac .* 0.10093;
            HerbFracGPP = HerbFrac .* 0.096378;
            TotFracGPP = TreeFracGPP + ShrubFracGPP + HerbFracGPP;
            TreeFracGPP = TreeFracGPP./TotFracGPP;
            ShrubFracGPP = ShrubFracGPP./TotFracGPP;
            HerbFracGPP = HerbFracGPP./TotFracGPP;

            Growtheff = HerbFracGPP .* Growtheffherb +  ShrubFracGPP .* Growtheffshrub +  TreeFracGPP .* Growthefftree; %starting assumption is all are 0.5 from above
            TotNPP = GPPstack .* Growtheff ./ Ccontent; %0.5 groth eff and also convert to biomass units here
            HerbNPP = TotNPP .* HerbFracGPP; %So NPP is weighted by fraction of canopy * a weighting coef determined by tuning coefs
            ShrubNPP = TotNPP .* ShrubFracGPP;  %
            TreeNPP = TotNPP .* TreeFracGPP;  %

            %Deal with leaves first
            %Herb assumes grass leaf NPP is dominated by total grass NPP and solve
            %starting from NPP
            %Partition NPP into leaf and froot for grsses
            NPPworking = nansum(HerbNPP(:,:,1:12),3); %Calculate annual WY Herb NPP
            NPPworking = NPPworking.* Fherbleaf; %Start with assumption root:shoot = 0.5
            NPPherbleaf = 0.* HerbNPP;
            NPPherbleaf(:,:,1:4) = 0; %Assume no herb leaf growth Oct-Jan
            NPPherbleaf(:,:,8:15) = 0; %Assume no herb leaf growth May-Dec
            NPPherbleaf(:,:,5) = NPPworking./3; %spread out NPP into Feb-April
            NPPherbleaf(:,:,6) = NPPworking./3; %spread out NPP
            NPPherbleaf(:,:,7) = NPPworking./3; %spread out NPP

            NPPworking = nansum(HerbNPP(:,:,1:12),3); %Calculate annual WY Herb NPP
            Deathworking = NPPworking.* Fherbleaf; %Start with assumption root:shoot = 0.5
            Deathherbleaf = 0.* HerbNPP;
            Deathherbleaf(:,:,1:7) = 0; %Assume no herb death
            Deathherbleaf(:,:,9:15) = 0; %Assume no herb death
            Deathherbleaf(:,:,8) = Deathworking; %assume all herb death occurs in May

            NPPherbroot = NPPherbleaf;%So fine root is 1/2 of NPP
            Deathherbroot = Deathherbleaf;

            %And herb writes
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_TotNPPherb_' num2str(years(ii)) '.mat']
            save(Outname,'HerbNPP','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPherbleaf_' num2str(years(ii)) '.mat']
            save(Outname,'NPPherbleaf','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPherbroot_' num2str(years(ii)) '.mat']
            save(Outname,'NPPherbroot','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathherbleaf_' num2str(years(ii)) '.mat']
            save(Outname,'Deathherbleaf','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathherbroot_' num2str(years(ii)) '.mat']
            save(Outname,'Deathherbroot','-v7.3')

            clear TotNPPherb HerbNPP NPPherbleaf NPPherbroot Deathherbleaf Deathherbroot;
            clear soilstack NIRv NDVI LAIevrg LAIherb GPPevrg GPPherb Fullstack %A few more clears to empty RAM

            %Trees next - this one starts from observed canopy cover and calculates leaf NPP based on observed LAI and
            %specified SLA and turnover time - this keeps #s from getting crazy
            NPPworking = LAItree .* SLeafwtevrg .* Kleafevrgyear; %This is annual

            NPPtreeleaf = TreeNPP./nansum(TreeNPP(:,:,1:12),3); %Fraction of NPP that happens in each wy month
            NPPtreeleaf = NPPtreeleaf .* NPPworking; %Absolute amount of leaf NPP in each wy month
            NPPtreeleaf(:,:,13:15) = NPPtreeleaf(:,:,1:3);

            Deathworking = nansum(NPPtreeleaf(:,:,1:12),3); %Just set this = NPP
            Deathtreeleaf = 0.* NPPtreeleaf;
            Deathtreeleaf(:,:,1) = Deathworking./3; %All leaf death occurs in Oct-Dec
            Deathtreeleaf(:,:,2) = Deathworking./3;
            Deathtreeleaf(:,:,3) = Deathworking./3;
            Deathtreeleaf(:,:,13) = Deathworking./3; %Just duplicate
            Deathtreeleaf(:,:,14) = Deathworking./3;
            Deathtreeleaf(:,:,15) = Deathworking./3;

            Leaftree = ones(size(NPPtreeleaf));
            Leaftree = Leaftree.* LAItree .* SLeafwtevrg;
            Leaftree = Leaftree + NPPtreeleaf - Deathtreeleaf;

            NPPtreefroot = NPPtreeleaf .* Fevergreenfroot;
            Deathtreefroot = Deathtreeleaf .* Fevergreenfroot;
            Froottree = Leaftree .* Fevergreenfroot;

            Leafevrg = Leaftree;
            Frootevrg = Froottree;

            %And write things out - Tree writes
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPtreetotal_' num2str(years(ii)) '.mat']
            save(Outname,'TreeNPP','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPtreeleaf_' num2str(years(ii)) '.mat']
            save(Outname,'NPPtreeleaf','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtreeleaf_' num2str(years(ii)) '.mat']
            save(Outname,'Deathtreeleaf','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Leaftree_' num2str(years(ii)) '.mat']
            save(Outname,'Leaftree','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPtreefroot_' num2str(years(ii)) '.mat']
            save(Outname,'NPPtreefroot','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtreefroot_' num2str(years(ii)) '.mat']
            save(Outname,'Deathtreefroot','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Froottree_' num2str(years(ii)) '.mat']
            save(Outname,'Froottree','-v7.3')

            clear NPPtreeleaf Deathtreeleaf Leaftree NPPtreefroot Deathtreefroot Froottree

            %And Shrubs next - as with trees, this one starts from observed canopy cover and calculates leaf NPP based on observed LAI and
            %specified SLA and turnover time
            NPPworking = LAIshrub .* SLeafwtevrg .* Kleafevrgyear; %This is annual

            NPPshrubleaf = ShrubNPP./nansum(ShrubNPP(:,:,1:12),3); %Normaized by all 12 months
            NPPshrubleaf = NPPshrubleaf .* NPPworking;
            NPPshrubleaf(:,:,13:15) = NPPshrubleaf(:,:,1:3);

            Deathworking = nansum(NPPshrubleaf(:,:,1:12),3); %Calculate annual  NPP
            Deathshrubleaf = 0.* NPPshrubleaf;
            Deathshrubleaf(:,:,1) = Deathworking./3; %All leaf death occurs in Oct-Dec
            Deathshrubleaf(:,:,2) = Deathworking./3;
            Deathshrubleaf(:,:,3) = Deathworking./3;
            Deathshrubleaf(:,:,13) = Deathworking./3; %Just duplicate
            Deathshrubleaf(:,:,14) = Deathworking./3;
            Deathshrubleaf(:,:,15) = Deathworking./3;

            Leafshrub = ones(size(NPPshrubleaf));
            Leafshrub = Leafshrub.* LAIshrub .* SLeafwtevrg;
            Leafshrub = Leafshrub + NPPshrubleaf - Deathshrubleaf;

            NPPshrubfroot = NPPshrubleaf .* Fevergreenfroot;
            Deathshrubfroot = Deathshrubleaf .* Fevergreenfroot;
            Frootshrub = Leafshrub .* Fevergreenfroot;

            Leafevrg = Leafevrg + Leafshrub;
            Frootevrg = Frootevrg + Frootshrub;

            %And write things out - Shrub writes
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubtotal_' num2str(years(ii)) '.mat']
            save(Outname,'ShrubNPP','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubleaf_' num2str(years(ii)) '.mat']
            save(Outname,'NPPshrubleaf','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrubleaf_' num2str(years(ii)) '.mat']
            save(Outname,'Deathshrubleaf','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Leafshrub_' num2str(years(ii)) '.mat']
            save(Outname,'Leafshrub','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubfroot_' num2str(years(ii)) '.mat']
            save(Outname,'NPPshrubfroot','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrubfroot_' num2str(years(ii)) '.mat']
            save(Outname,'Deathshrubfroot','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Frootshrub_' num2str(years(ii)) '.mat']
            save(Outname,'Frootshrub','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Leafevrg_' num2str(years(ii)) '.mat']
            save(Outname,'Leafevrg','-v7.3')
            Outname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Frootevrg_' num2str(years(ii)) '.mat']
            save(Outname,'Frootevrg','-v7.3')
        end
        clear NPPshrubleaf Deathshrubleaf Leafshrub NPPshrubfroot Deathshrubfroot Frootshrub
    end

    %Second half of Block2 - live pools
    if 1 < 2; %Use this to skip second half of block if not needed

        %Read in distubance and veg layers - prepared by VegCorrector - 37
        %layers - should be synchronized - -1 to 0
        Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_deltaFracShrubstack.mat']
        load(Inname); %'deltaFracShrubstack'
        deltaFracShrubstackhist = deltaFracShrubstack;
        clear deltaFracShrubstack
        Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_preShrubstack.mat']
        load(Inname); %'preShrubstack'
        preShrubstackhist = preShrubstack;
        clear preShrubstackInname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_deltaFracTreestack.mat']
        Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_deltaFracTreestack.mat']
        load(Inname); %'deltaFracTreestack'
        deltaFracTreestackhist = deltaFracTreestack;
        clear deltaFracTreestack
        Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_preTreestack.mat']
        load(Inname); %'preTreestack'
        preTreestackhist = preTreestack;
        clear preTreestack

        % Read in disturbance layer – annual disturb type x canopy loss
        % 37 bands, one for each year from 1985 - 2021 (in order). At each pixel there will be some integer value between 1 and 5. It goes:
        Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_Disturbtype.mat'];
        load(Inname); %Disturbtype
%         Inname = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Disturbance/ARD_' ARDTile '_distType_5000px.tif']; %From Jon analysis stack of 36 for years  %New path
%         [Disturbtypehist] = geotiffread(Inname); %Just a patch for now
        Disturbtypehist = uint8(Disturbtype);
        clear Disturbtype

        %Calculate Ks
        Kleafevrgyear = 1/Tleafevrg; %Annual K
        Kleafevrgmonth = 1-(power(exp(-(Kleafevrgyear)), 1.0/12)); %Monthly K

        Ktree1hryear = Kcorrection.*1/Twood1hr; %Annual K
        Ktree10hryear = Kcorrection.*1/Twood10hr;
        Ktree100hryear = Kcorrection.*1/Twood100hr;
        Ktree1000hryear = Kcorrection.*1/Twood1000hr;
        Ktreetrunkyear = Kcorrection.*1/Twoodtrunk;

        Kshrub1hryear = Kcorrection.*1/Twood1hr; %Annual K
        Kshrub10hryear = Kcorrection.*1/Twood10hr;
        Kshrub100hryear = Kcorrection.*1/Twood100hr;

        Ktree1hrmonth = 1-(power(exp(-(Ktree1hryear)), 1.0/12)); %Monthly K
        Ktree10hrmonth = 1-(power(exp(-(Ktree10hryear)), 1.0/12));
        Ktree100hrmonth = 1-(power(exp(-(Ktree100hryear)), 1.0/12));
        Ktree1000hrmonth = 1-(power(exp(-(Ktree1000hryear)), 1.0/12));
        Ktreetrunkmonth = 1-(power(exp(-(Ktreetrunkyear)), 1.0/12));

        Kshrub1hrmonth = 1-(power(exp(-(Kshrub1hryear)), 1.0/12)); %Monthly K
        Kshrub10hrmonth = 1-(power(exp(-(Kshrub10hryear)), 1.0/12));
        Kshrub100hrmonth = 1-(power(exp(-(Kshrub100hryear)), 1.0/12));

        %for ii = startii:size(years,2); %Annual looper
        for ii = 1:size(years,2); %Annual looper
            %for ii = 37; %Annual looper

            %Calculate distubance and veg layers - prepared by VegCorrector
            Disturbfractionshrub = deltaFracShrubstackhist(:,:, ii);
            Disturbfractionshrub(isnan(Disturbfractionshrub)) = 0;
            preShrub = preShrubstackhist(:,:, ii);
            Disturbfractionshrub(preShrub == 0 | isnan(preShrub) | isnan(Disturbfractionshrub)) = 0;

            Disturbfractiontree = deltaFracTreestackhist(:,:, ii);
            preTree = preTreestackhist(:,:, ii);
            Disturbfractiontree(preTree == 0 | isnan(preTree) | isnan(Disturbfractiontree)) = 0;

            Disturbfractiontree = -Disturbfractiontree;
            Disturbfractiontree(isnan(Disturbfractiontree)) = 0;
            Disturbfractiontree(Disturbfractiontree < 0) = 0;
            Disturbfractiontree(Disturbfractiontree > 1) = 1; %Fractional disturbance for trees

            Disturbfractionshrub = -Disturbfractionshrub;
            Disturbfractionshrub(isnan(Disturbfractionshrub)) = 0;
            Disturbfractionshrub(Disturbfractionshrub < 0) = 0;
            Disturbfractionshrub(Disturbfractionshrub > 1) = 1; %Fractional disturbance for trees and shrubs

            Disturbfraction = (Disturbfractionshrub + Disturbfractiontree)./2;

            Disturbtype = Disturbtypehist(:,:,ii); %Just keep current year
            Disturbed = Disturbtype == 1 | Disturbtype == 2 | Disturbtype == 3 | Disturbtype == 5;
            Disturbtype(Disturbed) = 1;
            Disturbtype(~Disturbed) = 0; %A bit of a shorth term patch, so take all disturb attributions

            %Now we do longer lived pools
            %Tree pools sizes
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPtreetotal_' num2str(years(ii)) '.mat']
            load(Inname); %,'TreeNPP'
            Tree1hr = single(zeros(size(TreeNPP)));
            Tree10hr = Tree1hr;
            Tree100hr = Tree1hr;
            Tree1000hr = Tree1hr;
            Treetrunk = Tree1hr;
            Treeroot = Tree1hr;
            Treeleaf = Tree1hr;

            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_LAIshrub_' num2str(years(ii)) '.mat']
            load(Inname); %,'LAIshrub'
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_LAItree_' num2str(years(ii)) '.mat']
            load(Inname); %,'LAItree'

            %Initialize trees
            %Starts with average biomass for first 5 years from emapr
            %Then allocates this with same scheme used in main program
            %Note - some terminology here is different from in main pgm
            if ii == startii;
                if ~isempty(SubTile2run);
                    Inname = [indirectoryroot '/RAWInputs/Biomass/' 'emapr_' ARDTile '.mat'];
                    load(Inname); %Biomass
                else
                    Inname = [indirectoryroot '/RAWInputs/Biomass/' 'emapr_' ARDTile '.tif']; %emapR
                    [Biomass] = geotiffread(Inname);
                end

                Biomass = single(Biomass(:,:,1:5)); %First 5 years emaprR biomass, Mg/ha - assume all of this is in trees and aboveground
                Biomass = nanmean(Biomass,3);
                Biomass = Biomass.*100; %Convert to g/m2 % Need to double check - not really clar what emapr units are.....

                %This is the same basic allocation NPP scheme used for NPP below
                Leaftreemax = LAItree .* SLeafwtevrg;
                BasalAreaNeeded = real(10000* 0.0045*(Leaftreemax/1000).^0.8685); %cm2/m2, This is the basal area needed to support evergreen leaf mass, based onAllocation ideas.xlsx, Enquist, Niklas,

                %This next block figures out how much basal area and wood mass is needed to support the observed LAI
                Leafdemand = LAItree .* SLeafwtevrg;
                Wooddemand1hr = BasalAreaNeeded .* Allocate1hr; %g/m2, this is mass of 1 hr branch needed to support evergreen leaf mass, source as above
                Wooddemand10hr = BasalAreaNeeded .* Allocate10hr; %g/m2, needed for 10 hr, assumes Leandro branching rule and power rule based branch length
                Wooddemand100hr = BasalAreaNeeded .* Allocate100hr; %
                Wooddemand1000hr = BasalAreaNeeded .* Allocate1000hr; %
                Wooddemandtrunk = BasalAreaNeeded .* Allocatetrunk; %

                Biomassleftover = Biomass; %Havn't given any Biomass out yet

                Newleaf = Leafdemand; %Allocate Biomass to leaves first
                NotEnough = Leafdemand > Biomassleftover; %
                Newleaf(NotEnough) = Biomassleftover(NotEnough); %
                Biomassleftover = Biomassleftover - Newleaf; %Track and decrease available Biomass as it's given out to pools sequentially

                New1hr = Wooddemand1hr; %Allocate Biomass to leaves first
                NotEnough = Wooddemand1hr > Biomassleftover; %
                New1hr(NotEnough) = Biomassleftover(NotEnough); %
                Biomassleftover = Biomassleftover - New1hr; %Track and decrease available Biomass as it's given out to pools sequentially

                New10hr = Wooddemand10hr; %Allocate Biomass to leaves first
                NotEnough = Wooddemand10hr > Biomassleftover; %
                New10hr(NotEnough) = Biomassleftover(NotEnough); %
                Biomassleftover = Biomassleftover - New10hr; %Track and decrease available Biomass as it's given out to pools sequentially

                New100hr = Wooddemand100hr; %Allocate Biomass to leaves first
                NotEnough = Wooddemand100hr > Biomassleftover; %
                New100hr(NotEnough) = Biomassleftover(NotEnough); %
                Biomassleftover = Biomassleftover - New100hr; %Track and decrease available Biomass as it's given out to pools sequentially

                New1000hr = Wooddemand1000hr; %Allocate Biomass to leaves first
                NotEnough = Wooddemand1000hr > Biomassleftover; %
                New1000hr(NotEnough) = Biomassleftover(NotEnough); %
                Biomassleftover = Biomassleftover - New1000hr; %Track and decrease available Biomass as it's given out to pools sequentially

                Newtrunk = Wooddemandtrunk; %Allocate Biomass to leaves first
                NotEnough = Wooddemandtrunk > Biomassleftover; %
                Newtrunk(NotEnough) = Biomassleftover(NotEnough); %
                Biomassleftover = Biomassleftover - Newtrunk; %Track and decrease available Biomass as it's given out to pools sequentially

                % Dealing with excess - put into all wood size classes in
                % proprtion to allometry needs
                Allneeded = Wooddemand1hr + Wooddemand10hr + Wooddemand100hr + Wooddemand1000hr + Wooddemandtrunk;
                New1hr = New1hr + (Biomassleftover).*(Wooddemand1hr./(Allneeded)); %Left over NPP goes all pools in proportion to allometry
                New10hr = New10hr + (Biomassleftover).*(Wooddemand10hr./(Allneeded)); %Left over NPP goes all pools in proportion to allometry
                New100hr = New100hr + (Biomassleftover).*(Wooddemand100hr./(Allneeded)); %Left over NPP goes all pools in proportion to allometry
                New1000hr = New1000hr + (Biomassleftover).*(Wooddemand1000hr./(Allneeded)); %Left over NPP goes all pools in proportion to allometry
                Newtrunk = Newtrunk + (Biomassleftover).*(Wooddemandtrunk./(Allneeded)); %Left over NPP goes all pools in proportion to allometry

                %And copy results into pool locations
                Treeleaf(:,:,3) = LAItree .* SLeafwtevrg;
                Tree1hr(:,:,3) = New1hr;
                Tree10hr(:,:,3)  = New10hr;
                Tree100hr(:,:,3)  = New100hr;
                Tree1000hr(:,:,3) = New1000hr;
                Treetrunk(:,:,3) = Newtrunk;
                Treeroot(:,:,3) = (Tree1hr(:,:,3) + Tree10hr(:,:,3) + Tree100hr(:,:,3) + Tree1000hr(:,:,3) + Treetrunk(:,:,3)).* Fevergreenwoodyroot;
            end

            if ii > startii; %For subsuquent years just read in last year biomass
                Treeleaf(:,:,3) = LAItree .* SLeafwtevrg;

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Tree1hr_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Tree1hr(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Tree10hr_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Tree10hr(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Tree100hr_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Tree100hr(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Tree1000hr_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Tree1000hr(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Treetrunk_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Treetrunk(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Treeroot_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Treeroot(:,:,1:3) = oldfile.Wood(:,:,13:15);
            end

            %Tree allocate to stem size classes
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPtreetotal_' num2str(years(ii)) '.mat']
            load(Inname); %,'TreeNPP'
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPtreeleaf_' num2str(years(ii)) '.mat']
            load(Inname); %,'NPPtreeleaf'

            %Now figure out the NPP allocation needed for each pool
            %This starts with biomass in each pool at the end of last year
            NPPtotal = nansum(TreeNPP(:,:,4:15),3); %Calculate annual NPP

            %This next block figures out how much basal area and wood mass is needed to support the observed LAI
            Leaftreemax = LAItree .* SLeafwtevrg;
            BasalAreaNeeded = real(10000* 0.0045*(Leaftreemax/1000).^0.8685); %cm2/m2, This is the basal area needed to support evergreen leaf mass, based onAllocation ideas.xlsx, Enquist, Niklas,
            WoodNeeded1hr = BasalAreaNeeded .* Allocate1hr; %g/m2, this is mass of 1 hr branch needed to support evergreen leaf mass, source as above
            WoodNeeded10hr = BasalAreaNeeded .* Allocate10hr; %g/m2, needed for 10 hr, assumes Leandro branching rule and power rule based branch length
            WoodNeeded100hr = BasalAreaNeeded .* Allocate100hr; %
            WoodNeeded1000hr = BasalAreaNeeded .* Allocate1000hr; %
            WoodNeededtrunk = BasalAreaNeeded .* Allocatetrunk; %

            %Mass balance, calculate how much production is needed by each stem size class to support that LAI and account for death
            %Leafdemand = LAItree .* SLeafwtevrg - Treeleaf(:,:,3) + Treeleaf(:,:,3).* Kleafevrgyear;
            %Use this to be consistent with leaf production
            Leafdemand = nansum(NPPtreeleaf(:,:,4:15), 3);

            Wooddemand1hr = Tree1hr(:,:,3).* Ktree1hryear;
            Wooddemand10hr = Tree10hr(:,:,3).* Ktree10hryear;
            Wooddemand100hr = Tree100hr(:,:,3).* Ktree100hryear;
            Wooddemand1000hr = Tree1000hr(:,:,3).* Ktree1000hryear;
            Wooddemandtrunk = Treetrunk(:,:,3).* Ktreetrunkyear;
            %             Wooddemand1hr = WoodNeeded1hr - Tree1hr(:,:,3) + Tree1hr(:,:,3).* Ktree1hryear;
            %             Wooddemand10hr = WoodNeeded10hr - Tree10hr(:,:,3) + Tree10hr(:,:,3).* Ktree10hryear;
            %             Wooddemand100hr = WoodNeeded100hr - Tree100hr(:,:,3) + Tree100hr(:,:,3).* Ktree100hryear;
            %             Wooddemand1000hr = WoodNeeded1000hr - Tree1000hr(:,:,3) + Tree1000hr(:,:,3).* Ktree1000hryear;
            %             Wooddemandtrunk = WoodNeededtrunk - Treetrunk(:,:,3) + Treetrunk(:,:,3).* Ktreetrunkyear;
            %             Wooddemand1hr(Wooddemand1hr < 0) = 0;
            %             Wooddemand10hr(Wooddemand10hr < 0) = 0;
            %             Wooddemand100hr(Wooddemand100hr < 0) = 0;
            %             Wooddemand1000hr(Wooddemand1000hr < 0) = 0;
            %             Wooddemandtrunk(Wooddemandtrunk < 0) = 0;

            %Now allocate NPP based on demand, starting with smallest class and calculate what's left for the next class
            NPPleftover = NPPtotal; %Havn't given any NPP out yet

            Newleaf = Leafdemand; %Allocate NPP to leaves first
            NotEnough = (1+Fevergreenfroot).*Leafdemand > NPPleftover; %The accounts for woody roots - 1 goes to stem and another .5 to woody roots
            Newleaf(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenfroot); %Check if there wasn't enough left to meet the 1 hr demand
            Newfroot = Newleaf .* Fevergreenfroot; %fine roots set = leaves/2
            NPPleftover = NPPleftover - Newleaf - Newfroot; %Track and decrease available NPP as it's given out to pools sequentially

            Newwood1hr = Wooddemand1hr; %Allocate NPP to smallest branches first
            NotEnough = (1+Fevergreenfroot).*Wooddemand1hr > NPPleftover; %
            Newwood1hr(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenfroot); %Check if there wasn't enough left to meet the 1 hr demand
            NPPleftover = NPPleftover - Newwood1hr - Newwood1hr .* Fevergreenwoodyroot; %Track and decrease available NPP as it's given out to pools sequentially

            Newwood10hr = Wooddemand10hr; %Allocate NPP to smallest branches first
            NotEnough = (1+Fevergreenfroot).*Wooddemand10hr > NPPleftover; %
            Newwood10hr(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenfroot); %Check if there wasn't enough left to meet the 1 hr demand
            NPPleftover = NPPleftover - Newwood10hr - Newwood10hr .* Fevergreenwoodyroot; %Track and decrease available NPP as it's given out to pools sequentially

            Newwood100hr = Wooddemand100hr; %Allocate NPP to smallest branches first
            NotEnough = (1+Fevergreenfroot).*Wooddemand100hr > NPPleftover; %
            Newwood100hr(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenfroot); %Check if there wasn't enough left to meet the 1 hr demand
            NPPleftover = NPPleftover - Newwood100hr - Newwood100hr .* Fevergreenwoodyroot; %Track and decrease available NPP as it's given out to pools sequentially

            Newwood1000hr = Wooddemand1000hr; %Allocate NPP to smallest branches first
            NotEnough = (1+Fevergreenfroot).*Wooddemand1000hr > NPPleftover; %
            Newwood1000hr(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenfroot); %Check if there wasn't enough left to meet the 1 hr demand
            NPPleftover = NPPleftover - Newwood1000hr - Newwood1000hr .* Fevergreenwoodyroot; %Track and decrease available NPP as it's given out to pools sequentially

            Newwoodtrunk = Wooddemandtrunk; %Allocate NPP to smallest branches first
            NotEnough = (1+Fevergreenfroot).*Wooddemandtrunk > NPPleftover; %
            Newwoodtrunk(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenfroot); %Check if there wasn't enough left to meet the 1 hr demand
            NPPleftover = NPPleftover - Newwoodtrunk - Newwoodtrunk .* Fevergreenwoodyroot; %Track and decrease available NPP as it's given out to pools sequentially

            %Dealing with excess NPP - put into all wood size classes in
            %proportion to allometry *need* (not turnover related *demand*)
            Allwoodneeded = WoodNeeded1hr + WoodNeeded10hr + WoodNeeded100hr + WoodNeeded1000hr + WoodNeededtrunk;
            Newwood1hr = Newwood1hr + (NPPleftover.*(1/(1+ Fevergreenwoodyroot))).*(WoodNeeded1hr./(Allwoodneeded)); %Left over NPP goes all pools in proportion to allometry
            Newwood10hr = Newwood10hr + (NPPleftover.*(1/(1+ Fevergreenwoodyroot))).*(WoodNeeded10hr./(Allwoodneeded)); %
            Newwood100hr = Newwood100hr + (NPPleftover.*(1/(1+ Fevergreenwoodyroot))).*(WoodNeeded100hr./(Allwoodneeded)); %
            Newwood1000hr = Newwood1000hr + (NPPleftover.*(1/(1+ Fevergreenwoodyroot))).*(WoodNeeded1000hr./(Allwoodneeded)); %
            Newwoodtrunk = Newwoodtrunk + (NPPleftover.*(1/(1+ Fevergreenwoodyroot))).*(WoodNeededtrunk./(Allwoodneeded)); %

            Newwoodroot = Newwood1hr .* Fevergreenwoodyroot; %Woody roots treated same as sum of new wood allocation
            Newwoodroot = Newwoodroot + Newwood10hr .* Fevergreenwoodyroot; %
            Newwoodroot = Newwoodroot + Newwood100hr .* Fevergreenwoodyroot; %
            Newwoodroot = Newwoodroot + Newwood1000hr .* Fevergreenwoodyroot; %
            Newwoodroot = Newwoodroot + Newwoodtrunk .* Fevergreenwoodyroot; %

            Ftreeleaf = Newleaf./NPPtotal; %And then calculate the fractions allocated to each pool - these get used later to actually calc mass balances
            Ftree1hr = Newwood1hr./NPPtotal; %
            Ftree10hr = Newwood10hr./NPPtotal;
            Ftree100hr = Newwood100hr./NPPtotal;
            Ftree1000hr = Newwood1000hr./NPPtotal;
            Ftreetrunk = Newwoodtrunk./NPPtotal;
            Ftreeroot = Newwoodroot./NPPtotal;
            Ftreefroot = Newfroot./NPPtotal;

            NPPtreewood = TreeNPP;       %Need to save this for use in pool tracker

            %And on to shrubs - set up pools sizes
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubtotal_' num2str(years(ii)) '.mat']
            load(Inname); %,'ShrubNPP'
            Shrub1hr = single(zeros(size(ShrubNPP)));
            Shrub10hr = Shrub1hr;
            Shrub100hr = Shrub1hr;
            Shrubroot = Shrub1hr;
            Shrubleaf = Shrub1hr;

            %Initialize shrub pool sizes
            %Differs from tree initilization - we don't have emapr on initial
            %total biomss.  Do the following: 1) calculate mean shrub woody
            %NPP for first 5 years, 2) calculate steady state total
            %biomass based on this and allometry weighted turnover, 3)
            %allocate this biomass based on allometry.
            if ii == startii;
                ShrubNPPinit = []; %Determine abovegnd woody NPP (non leaf and fine root) for first 5 years
                for pp = 4:9
                    %for pp = 1
                    Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubtotal_' num2str(years(pp)) '.mat']
                    load(Inname); %,'ShrubNPP'
                    NPP1 = nansum(ShrubNPP(:,:,4:15), 3);
                    Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubleaf_' num2str(years(pp)) '.mat']
                    load(Inname); %,'NPPshrubleaf'
                    NPP2 = nansum(NPPshrubleaf(:,:,4:15), 3).* (1+Fevergreenfroot);
                    NPP3 = (NPP1-NPP2)./(1+Fevergreenwoodyroot);
                    ShrubNPPinit = cat(3, ShrubNPPinit, NPP3);
                end
                ShrubNPPinit = nanmean(ShrubNPPinit,3); %Just woody

                AllocateTotal = (Allocate1hr + Allocate10hr + Allocate100hr); %Calculate mass weighted lifetime for a well-designed shrub
                Weightedlifetime = (AllocateTotal)/(Kshrub1hryear*Allocate1hr + Kshrub10hryear*Allocate10hr + Kshrub100hryear*Allocate100hr);

                Steadybiomass = ShrubNPPinit .* Weightedlifetime; %Calculate steady state biomass

                %And allocate to pools
                Shrubleaf(:,:,3) = LAIshrub .* SLeafwtevrg;
                Shrub1hr(:,:,3) = Steadybiomass .* Allocate1hr ./ AllocateTotal;
                Shrub10hr(:,:,3) = Steadybiomass .* Allocate10hr ./ AllocateTotal;
                Shrub100hr(:,:,3) = Steadybiomass .* Allocate100hr ./ AllocateTotal;
                Shrubroot(:,:,3) = (Shrub1hr(:,:,3) + Shrub10hr(:,:,3) + Shrub100hr(:,:,3)).* Fevergreenwoodyroot;
            end

            if ii > startii; %For subsuquent years just read in last year biomass
                Shrubleaf(:,:,3) = LAIshrub .* SLeafwtevrg;

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Shrub1hr_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Shrub1hr(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Shrub10hr_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Shrub10hr(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Shrub100hr_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Shrub100hr(:,:,1:3) = oldfile.Wood(:,:,13:15);

                Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Shrubroot_' num2str(years(ii-1)) '.mat']
                oldfile = load(Inname); %,'Wood'
                oldfile.Wood(isnan(oldfile.Wood)) = 0;
                Shrubroot(:,:,1:3) = oldfile.Wood(:,:,13:15);
            end

            %Now figure out the NPP allocation needed for each shrub pool
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubtotal_' num2str(years(ii)) '.mat']
            load(Inname); %,'ShrubNPP'
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_NPPshrubleaf_' num2str(years(ii)) '.mat']
            load(Inname); %,'NPPshrubleaf'
            %This starts with biomass in each pool at the end of last year

            NPPtotal = nansum(ShrubNPP(:,:,4:15),3); %Calculate annual total NPP for the year

            %This next block figures out how much basal area and wood mass is needed to support the observed LAI
            Leafshrubmax = LAIshrub .* SLeafwtevrg;
            BasalAreaNeeded = real(10000* 0.0045*(Leafshrubmax/1000).^0.8685); %cm2/m2, This is the basal area needed to support evergreen leaf mass, based onAllocation ideas.xlsx, Enquist, Niklas,
            WoodNeeded1hr = BasalAreaNeeded .* Allocate1hr; %g/m2, this is mass of 1 hr branch needed to support evergreen leaf mass, source as above
            WoodNeeded10hr = BasalAreaNeeded .* Allocate10hr; %g/m2, needed for 10 hr, assumes Leandro branching rule and power rule based branch length
            WoodNeeded100hr = BasalAreaNeeded .* Allocate100hr; %

            %Mass balance, calculate how much production is needed by each stem size class to support that LAI and account for death
            %Leafdemand = LAIshrub .* SLeafwtevrg - Shrubleaf(:,:,3) + Shrubleaf(:,:,3).* Kleafevrgyear;
            %Use this to be consistent with NPP leafN
            Leafdemand = nansum(NPPshrubleaf(:,:,4:15), 3);

            Wooddemand1hr = (Shrub1hr(:,:,3).* Kshrub1hryear);
            Wooddemand10hr = (Shrub10hr(:,:,3).* Kshrub10hryear);
            Wooddemand100hr = (Shrub100hr(:,:,3).* Kshrub100hryear);
            %             Wooddemand1hr = WoodNeeded1hr - Shrub1hr(:,:,3) + (Shrub1hr(:,:,3).* Kshrub1hryear);
            %             Wooddemand10hr = WoodNeeded10hr - Shrub10hr(:,:,3) + (Shrub10hr(:,:,3).* Kshrub10hryear);
            %             Wooddemand100hr = WoodNeeded100hr - Shrub100hr(:,:,3) + (Shrub100hr(:,:,3).* Kshrub100hryear);
            %             Wooddemand1hr(Wooddemand1hr < 0) = 0;
            %             Wooddemand10hr(Wooddemand10hr < 0) = 0;
            %             Wooddemand100hr(Wooddemand100hr < 0) = 0;

            %Now allocate NPP based on demand, starting with smallest class and calculate what's left for the next class
            NPPleftover = NPPtotal; %Havn't given any NPP out yet

            Newleaf = Leafdemand; %Allocate NPP to leaves first
            NotEnough = (1+Fevergreenfroot).*Leafdemand > NPPleftover; %The factor 2 is for woody roots - 1 goes to stem and another 1 to woody roots
            Newleaf(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenfroot); %Check if there wasn't enough left to meet the 1 hr demand
            Newfroot = Newleaf .* Fevergreenfroot; %fine roots set = leaves/2
            NPPleftover = NPPleftover - (Newleaf * (1+Fevergreenfroot)); %Track and decrease available NPP as it's given out to pools sequentially

            Newwood1hr = Wooddemand1hr; %Allocate NPP to smallest branches first
            NotEnough = (1+Fevergreenwoodyroot).*Wooddemand1hr > NPPleftover; %The factor 2 is for woody roots - 1 goes to stem and another 1 to woody roots
            Newwood1hr(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenwoodyroot); %Check if there wasn't enough left to meet the 1 hr demand
            NPPleftover = NPPleftover - (Newwood1hr .* (1+Fevergreenwoodyroot)); %Track and decrease available NPP as it's given out to pools sequentially

            Newwood10hr = Wooddemand10hr; %Same deal, moving on to 10 hr fuels
            NotEnough = (1+Fevergreenwoodyroot).*Wooddemand10hr > NPPleftover;
            Newwood10hr(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenwoodyroot);
            NPPleftover = NPPleftover - (Newwood10hr .* (1+Fevergreenwoodyroot));

            Newwood100hr = Wooddemand100hr; %Same deal, moving on to 100 hr fuels
            NotEnough = (1+Fevergreenwoodyroot).*Wooddemand100hr > NPPleftover;
            Newwood100hr(NotEnough) = (NPPleftover(NotEnough))./(1+Fevergreenwoodyroot);
            NPPleftover = NPPleftover - (Newwood100hr .* (1+Fevergreenwoodyroot));

            %Dealing with excess NPP - put into all wood size classes in
            %proportion to allometry *need* (not turnover related *demand*)
            Totalneeded = (WoodNeeded1hr + WoodNeeded10hr + WoodNeeded100hr);
            Wood2give = NPPleftover./(1+Fevergreenwoodyroot);
            Newwood1hr = Newwood1hr + Wood2give.*WoodNeeded1hr./Totalneeded; %Left over NPP goes all pools in proportion to allometry
            Newwood10hr = Newwood10hr + Wood2give.*WoodNeeded10hr./Totalneeded; %Left over NPP goes all pools in proportion to allometry
            Newwood100hr = Newwood100hr + Wood2give.*WoodNeeded100hr./Totalneeded; %Left over NPP goes all pools in proportion to allometry
            Newwoodroot = (Newwood1hr + Newwood10hr + Newwood100hr) .* Fevergreenwoodyroot; %Woody roots treated same as sum of 1, 10, 100 fuels

            Fshrubleaf = Newleaf./NPPtotal; %And then calculate the fractions allocated to each pool - these get used later to actually calc mass balances
            Fshrub1hr = Newwood1hr./NPPtotal;
            Fshrub10hr = Newwood10hr./NPPtotal;
            Fshrub100hr = Newwood100hr./NPPtotal;
            Fshrubroot = Newwoodroot./NPPtotal;
            Fshrubfroot = Newfroot./NPPtotal;

            NPPshrubwood = ShrubNPP;  %This is total shrub NPP - not just wood - 15 month stack

            %This next block repeated many times - just mass balance accounting
            %Allocate and adjust live pools with death and ouput to litter

            %Calculate root turnover times based on weighted mean of aboveground size classes
            TotalAGB = Tree1hr(:,:,3) + Tree10hr(:,:,3) + Tree100hr(:,:,3) + Tree1000hr(:,:,3) + Treetrunk(:,:,3);
            Ktreerootmonth = Ktree1hrmonth.*Tree1hr(:,:,3)./TotalAGB + Ktree10hrmonth.*Tree10hr(:,:,3)./TotalAGB +Ktree100hrmonth.*Tree100hr(:,:,3)./TotalAGB +Ktree1000hrmonth.*Tree1000hr(:,:,3)./TotalAGB +Ktreetrunkmonth.*Treetrunk(:,:,3)./TotalAGB;

            TotalAGB = Shrub1hr(:,:,3) + Shrub10hr(:,:,3) + Shrub100hr(:,:,3);
            Kshrubrootmonth = Kshrub1hrmonth.*Shrub1hr(:,:,3)./TotalAGB + Kshrub10hrmonth.*Shrub10hr(:,:,3)./TotalAGB +Kshrub100hrmonth.*Shrub100hr(:,:,3)./TotalAGB;

            %PoolTracker(Disturbclass, NPPtotalwood, Fwood, Wood, Kwood, NPPname, Woodname, Deathname, Disturbname, Disturbfraction, Disturbtype, Disturbancekills, outdirectoryroot, ARDTile, ii, years);
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 1, Disturbtype, Disturbfractiontree, NPPtreewood, Ftree1hr, Tree1hr, Ktree1hrmonth, '_NPPtree1hr_', '_Tree1hr_', '_Deathtree1hr_', '_Disturbtree1hr_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 2, Disturbtype, Disturbfractiontree, NPPtreewood, Ftree10hr, Tree10hr, Ktree10hrmonth, '_NPPtree10hr_', '_Tree10hr_', '_Deathtree10hr_', '_Disturbtree10hr_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 3, Disturbtype, Disturbfractiontree, NPPtreewood, Ftree100hr, Tree100hr, Ktree100hrmonth, '_NPPtree100hr_', '_Tree100hr_', '_Deathtree100hr_', '_Disturbtree100hr_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 4, Disturbtype, Disturbfractiontree, NPPtreewood, Ftree1000hr, Tree1000hr, Ktree1000hrmonth, '_NPPtree1000hr_', '_Tree1000hr_', '_Deathtree1000hr_', '_Disturbtree1000hr_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 5, Disturbtype, Disturbfractiontree, NPPtreewood, Ftreetrunk, Treetrunk, Ktreetrunkmonth, '_NPPtreetrunk_', '_Treetrunk_', '_Deathtreetrunk_', '_Disturbtreetrunk_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 6, Disturbtype, Disturbfractiontree, NPPtreewood, Ftreeroot, Treeroot, Ktreerootmonth, '_NPPtreeroot_', '_Treeroot_', '_Deathtreeroot_', '_Disturbtreeroot_');

            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 1, Disturbtype, Disturbfractionshrub, NPPshrubwood, Fshrub1hr, Shrub1hr, Kshrub1hrmonth, '_NPPshrub1hr_', '_Shrub1hr_', '_Deathshrub1hr_', '_Disturbshrub1hr_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 2, Disturbtype, Disturbfractionshrub, NPPshrubwood, Fshrub10hr, Shrub10hr, Kshrub10hrmonth, '_NPPshrub10hr_', '_Shrub10hr_', '_Deathshrub10hr_', '_Disturbshrub10hr_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 3, Disturbtype, Disturbfractionshrub, NPPshrubwood, Fshrub100hr, Shrub100hr, Kshrub100hrmonth, '_NPPshrub100hr_', '_Shrub100hr_', '_Deathshrub100hr_', '_Disturbshrub100hr_');
            PoolTrackerWood(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 6, Disturbtype, Disturbfractionshrub, NPPshrubwood, Fshrubroot, Shrubroot, Kshrubrootmonth, '_NPPshrubroot_', '_Shrubroot_', '_Deathshrubroot_', '_Disturbshrubroot_');

            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Leafevrg_' num2str(years(ii)) '.mat']
            load(Inname); %'Leafevrg'
            Inname = [outdirectoryroot '/IntegrationModel/Block2/' ARDTile '_Frootevrg_' num2str(years(ii)) '.mat']
            load(Inname); %'Frootevrg'
            %global  Disturbancekills outdirectoryroot ARDTile ii years
            PoolTrackerLeaves(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 7, Disturbtype, Disturbfraction, Leafevrg, '_Disturbevrgleaf_')
            PoolTrackerLeaves(Disturbancekills, ARDTile, ii, years, outdirectoryroot, 7, Disturbtype, Disturbfraction, Frootevrg, '_Disturbevrgfroot_')
        end
        clearvars -except Kcorrection Blocks2run SubTile2run SubTile Ccontent startii ii ARD* *directoryroot *ear* Months2run mnth Fh* Fe* Growtheff* SLe* T* Allo* Disturbancekills
    end
end

%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%
%Block 3: Detritus and Fuels Pools and Fluxes
%CASA decomp and crosswalk to fuel models
%Borrows from Python version of CASA GFED 4 from JTR, July 2020

%%%Setups
indirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile]; %New path
outdirectoryroot = ['/dfs3a/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile]; %New path

clearvars -except  Blocks2run SubTile2run SubTile ii startii ARDTile ARDTile indirectoryroot outdirectoryroot Beginyear Endyear years Years2run Months2run mnth TCWD1hr TCWD10hr TCWD100hr TCWD1000hr TCWDtrunk TCWDroot Tleafevrg Tleafherb Tfroot KCWD1hr KCWD10hr KCWD100hr KCWD1000hr KCWDtrunk KCWDroot Kleafevrg Kleafherb Kfroot Freshslowdown AgeTCWD1hr AgeTCWD10hr AgeTCWD100hr AgeTCWD1000hr AgeTCWDtrunk AgeTCWDroot AgeKCWD1hr AgeKCWD10hr AgeKCWD100hr AgeKCWD1000hr AgeKCWDtrunk AgeKCWDroot Disturbfractremoval

%Decompostion lifetime
%Original #s
% TCWD1hr = 8.3; %years - estimated from FFE annual loss rates
% TCWD10hr = 8.3; %Mostly from FVS FFE https://www.fs.fed.us/.ftproot/pub/fmsc/ftp/fvs/docs/gtr/FFEguide.pdf
% TCWD100hr = 11.1; %See Table 2.4.6
% TCWD1000hr = 66.7;
% TCWDtrunk = 66.7;
% TCWDroot = 11.1;
% Tleafevrg = 2; %Seems low for oak and pine
% Tleafherb = .5;
% Tfroot = .5;

% Results from Tuning coefs - based on modeled litter input, observed siturbance, modeled combustion and observed pool
% sizes in NAWFD (https://fuels.mtri.org/) - intended to match data in NAWFD, and won't necessarily match biophysical processes
% Turnover times (yrs) are:  2.4017    4.4028    5.6329   11.3874   80.6479
% 7.1607 - run 7/16/21
TCWD1hr = 2.4; %years - these are to match observations, the wood are probably too fast, since obs are for above litter layer - this # is probably for above litter, andthen there's more time in actual litter
TCWD10hr =     4.4; %
TCWD100hr =     5.6; %
TCWD1000hr =    11.4;
TCWDtrunk =    80.6;
TCWDroot = 5; %Just guessing
Tleafevrg =     7.2; %This is to match the litter mass - it's probably too long for real leaf decomp, since litter has other pieces, including twigs
Tleafherb = .5; %Just guessing
Tfroot = .5; %Just guessing

Freshslowdown = 0.5; %The rate that fresh material decays relative to nonfresh - Just a guess

%Litter aging - rate that Fresh pools change into not fresh
AgeTCWD1hr = 5; %Based mostly on time takes to fall - Snag breakage and crown loss
AgeTCWD10hr = 8; %Table 2.4.5 in FVS FFE
AgeTCWD100hr = 12.5;
AgeTCWD1000hr = 18;
AgeTCWDtrunk = 25; %Table 4.20.2 in FVS FFE
AgeTCWDroot = 1;  %Just a guess

Kagewood1hryear = 1/AgeTCWD1hr; %Annual K
Kagewood10hryear = 1/AgeTCWD10hr;%
Kagewood100hryear = 1/AgeTCWD100hr;%
Kagewood1000hryear = 1/AgeTCWD1000hr;%
Kagewoodtrunkyear = 1/AgeTCWDtrunk;%
Kagewoodrootyear = 1/AgeTCWDroot;%

Kdecompwood1hryear = 1/TCWD1hr; %Annual K
Kdecompwood10hryear = 1/TCWD10hr;%
Kdecompwood100hryear = 1/TCWD100hr;%
Kdecompwood1000hryear = 1/TCWD1000hr;%
Kdecompwoodtrunkyear = 1/TCWDtrunk;%
Kdecompwoodrootyear = 1/TCWDroot;%
Kdecompleafevrgyear = 1/Tleafevrg;%
Kdecompleafherbyear = 1/Tleafherb;%
Kdecompfrootyear = 1/Tfroot;%

Kagewood1hrmonth = 1-(power(exp(-(1/AgeTCWD1hr)), 1.0/12)); %Monthly K
Kagewood10hrmonth = 1-(power(exp(-(1/AgeTCWD10hr)), 1.0/12)); %Monthly K
Kagewood100hrmonth = 1-(power(exp(-(1/AgeTCWD100hr)), 1.0/12)); %Monthly K
Kagewood1000hrmonth = 1-(power(exp(-(1/AgeTCWD1000hr)), 1.0/12)); %Monthly K
Kagewoodtrunkmonth = 1-(power(exp(-(1/AgeTCWDtrunk)), 1.0/12)); %Monthly K
Kagewoodrootmonth = 1-(power(exp(-(1/AgeTCWDroot)), 1.0/12)); %Monthly K

Kdecompwood1hrmonth = 1-(power(exp(-(1/TCWD1hr)), 1.0/12)); %Monthly K
Kdecompwood10hrmonth = 1-(power(exp(-(1/TCWD10hr)), 1.0/12)); %Monthly K
Kdecompwood100hrmonth = 1-(power(exp(-(1/TCWD100hr)), 1.0/12)); %Monthly K
Kdecompwood1000hrmonth = 1-(power(exp(-(1/TCWD1000hr)), 1.0/12)); %Monthly K
Kdecompwoodtrunkmonth = 1-(power(exp(-(1/TCWDtrunk)), 1.0/12)); %Monthly K
Kdecompwoodrootmonth = 1-(power(exp(-(1/TCWDroot)), 1.0/12)); %Monthly K
Kdecompleafevrgmonth = 1-(power(exp(-(1/Tleafevrg)), 1.0/12)); %Monthly K
Kdecompleafherbmonth = 1-(power(exp(-(1/Tleafherb)), 1.0/12)); %Monthly K
Kdecompfrootmonth = 1-(power(exp(-(1/Tfroot)), 1.0/12)); %Monthly K

%Disturbance Removal
%3 column x 24 row table of fraction dead pools lost in disturbance,
%Scalers that get multiplied by the canopy loss - a value of 0 means no detritus loss
%regardless of canopy loss, a value of 1 means detritus loss for that pool is
%directly proportional to canopy loss, and value of 2 means greater loss
%for that pool than canopy loss (for example, leaf litter)
%Columns are fire, management, dieoff
%24 rows for different pools
%Row 1 CWD1hr, Row 2 CWD10hr,Row 3 CWD100hr, Row 4 CWD1000hr,
%Row 5 CWDtrunk, Row 6 Root, Row 7 is evrgleaf, Row 8 is herbleaf
%1-8 were killed during that disturbance
%9-16 died earlier but are still in fresh pool
%17-24 are in aged pools
Disturbfractremoval = [1 1  0; 1 1  0; 0 1  0; 0 1  0; 0 1  0; 0 0  0; 1 1  0; 2 0  0;
    2 0 0; 2 0  0; 1 0  0; 1 0  0; 0.5 0  0; 0.5 0  0; 2 0  0; 2 0  0;
    2 0 0; 2 0  0; 1 0  0; 1 0  0; 0.5 0  0; 0.5 0  0; 2 0  0; 2 0  0]

%Read in distubance and veg layers - prepared by VegCorrector
%Read in distubance and veg layers - prepared by VegCorrector
Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_deltaFracShrubstack.mat']
load(Inname); %'deltaFracShrubstack'
deltaFracShrubstackhist = deltaFracShrubstack;
clear deltaFracShrubstack
Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_preShrubstack.mat']
load(Inname); %'preShrubstack'
preShrubstackhist = preShrubstack;
clear preShrubstackInname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_deltaFracTreestack.mat']
Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_deltaFracTreestack.mat']
load(Inname); %'deltaFracTreestack'
deltaFracTreestackhist = deltaFracTreestack;
clear deltaFracTreestack
Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_preTreestack.mat']
load(Inname); %'preTreestack'
preTreestackhist = preTreestack;
clear preTreestack

% Read in disturbance layer – annual disturb type x canopy loss
% 37 bands, one for each year from 1985 - 2021 (in order). At each pixel there will be some integer value between 1 and 5. It goes:
Inname = [indirectoryroot '/RAWInputs/Disturbance/ARD_' ARDTile '_Disturbtype.mat'];
load(Inname); %Disturbtype
%         Inname = ['/dfs4/jranders_lab/projects/CECS/Dataengine_C02/ARDTile' ARDTile '/RAWInputs/Disturbance/ARD_' ARDTile '_distType_5000px.tif']; %From Jon analysis stack of 36 for years  %New path
%         [Disturbtypehist] = geotiffread(Inname); %Just a patch for now
Disturbtypehist = uint8(Disturbtype);
clear Disturbtype

%%%%%%%%%%
%This operates on 15 month blocks from previous year Oct to current year Dec
%Loop through the years
%Very similar to Block 2 ideas and layout
if logical(sum(Blocks2run == 3))
    %for ii = startii:size(years,2); %Annual looper
        for ii = 1:size(years,2); %Annual looper
        %for ii = 37; %Annual looper

        %Calculate annual disturbances
        Disturbfractionshrub = deltaFracShrubstackhist(:,:, ii);
        Disturbfractionshrub(isnan(Disturbfractionshrub)) = 0;
        Disturbfractiontree = deltaFracTreestackhist(:,:, ii);
        Disturbfractiontree(isnan(Disturbfractiontree)) = 0;

        preShrub = preShrubstackhist(:,:, ii);
        Disturbfractionshrub(preShrub == 0 | isnan(preShrub) | isnan(Disturbfractionshrub)) = 0;

        preTree = preTreestackhist(:,:, ii);
        Disturbfractiontree(preTree == 0 | isnan(preTree) | isnan(Disturbfractiontree)) = 0;

        Disturbfractiontree = -Disturbfractiontree;
        Disturbfractiontree(isnan(Disturbfractiontree)) = 0;
        Disturbfractiontree(Disturbfractiontree < 0) = 0;
        Disturbfractiontree(Disturbfractiontree > 1) = 1; %Fractional disturbance for trees

        Disturbfractionshrub = -Disturbfractionshrub;
        Disturbfractionshrub(isnan(Disturbfractionshrub)) = 0;
        Disturbfractionshrub(Disturbfractionshrub < 0) = 0;
        Disturbfractionshrub(Disturbfractionshrub > 1) = 1; %Fractional disturbance for trees and shrubs

        Disturbfraction = (Disturbfractionshrub + Disturbfractiontree)./2; %This is what gets used the sum of tree and shrub loss

            Disturbtype = Disturbtypehist(:,:,ii); %Just keep current year
            Disturbtype(Disturbtype == 5) = 3; %Inclued unattrib dieoff - this doesn't matter much anyway
            Disturbtype(Disturbtype == 4) = 0;

        %Read inputs to pools (aka dieoffs) - shrub and tree inputs are added
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtree1hr_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwood1hr = Deathwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrub1hr_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwood1hr = Deathwood1hr + Deathwood; %Shrub and trees are added here

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbtree1hr_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwood1hr = Disturbwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbshrub1hr_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwood1hr = Disturbwood1hr + Disturbwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtree10hr_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwood10hr = Deathwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrub10hr_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwood10hr = Deathwood10hr + Deathwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbtree10hr_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwood10hr = Disturbwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbshrub10hr_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwood10hr = Disturbwood10hr + Disturbwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtree100hr_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwood100hr = Deathwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrub100hr_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwood100hr = Deathwood100hr + Deathwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbtree100hr_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwood100hr = Disturbwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbshrub100hr_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwood100hr = Disturbwood100hr + Disturbwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtree1000hr_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwood1000hr = Deathwood; %No 1000 hr or trenk for shrub - just trees

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbtree1000hr_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwood1000hr = Disturbwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtreetrunk_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwoodtrunk = Deathwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbtreetrunk_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwoodtrunk = Disturbwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtreeroot_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwoodroot = Deathwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrubroot_' num2str(years(ii)) '.mat']
        load(Inname); %Deathwood
        Deathwoodroot = Deathwoodroot + Deathwood;

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbtreeroot_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwoodroot = Disturbwood;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Disturbshrubroot_' num2str(years(ii)) '.mat']
        load(Inname); %Disturbwood
        Disturbwoodroot = Disturbwoodroot + Disturbwood;

        %Need to determinine initial pools - start with analytical steady state calculation
        %Create pool structure and populate previous Dec (layer 3)
        FreshCWD1hr = single(zeros(size(Deathwood1hr)));
        FreshCWD10hr = FreshCWD1hr;
        FreshCWD100hr = FreshCWD1hr;
        FreshCWD1000hr = FreshCWD1hr;
        FreshCWDtrunk = FreshCWD1hr;
        FreshCWDroot = FreshCWD1hr;

        if ii == startii; %Year 1 calculate initial pool size based on ratio of inflow and turnover
            AnnualDeathwood1hr = nansum(Deathwood1hr(:,:,4:15),3);
            AnnualKtot1hr = Kdecompwood1hryear.*Freshslowdown + Kagewood1hryear; %Account for reduced decomp by fresh
            FreshCWD1hr(:,:,3) = AnnualDeathwood1hr./AnnualKtot1hr;

            AnnualDeathwood10hr = nansum(Deathwood10hr(:,:,4:15),3);
            AnnualKtot10hr = Kdecompwood10hryear.*Freshslowdown + Kagewood10hryear; %Account for reduced decomp by fresh
            FreshCWD10hr(:,:,3) = AnnualDeathwood10hr./AnnualKtot10hr;

            AnnualDeathwood100hr = nansum(Deathwood100hr(:,:,4:15),3);
            AnnualKtot100hr = Kdecompwood100hryear.*Freshslowdown + Kagewood100hryear; %Account for reduced decomp by fresh
            FreshCWD100hr(:,:,3) = AnnualDeathwood100hr./AnnualKtot100hr;

            AnnualDeathwood1000hr = nansum(Deathwood1000hr(:,:,4:15),3);
            AnnualKtot1000hr = Kdecompwood1000hryear.*Freshslowdown + Kagewood1000hryear; %Account for reduced decomp by fresh
            FreshCWD1000hr(:,:,3) = AnnualDeathwood1000hr./AnnualKtot1000hr;

            AnnualDeathwoodtrunk = nansum(Deathwoodtrunk(:,:,4:15),3);
            AnnualKtottrunk = Kdecompwoodtrunkyear.*Freshslowdown + Kagewoodtrunkyear; %Account for reduced decomp by fresh
            FreshCWDtrunk(:,:,3) = AnnualDeathwoodtrunk./AnnualKtottrunk;

            AnnualDeathwoodroot = nansum(Deathwoodroot(:,:,4:15),3);
            AnnualKtotroot = Kdecompwoodrootyear.*Freshslowdown + Kagewoodrootyear; %Account for reduced decomp by fresh
            FreshCWDroot(:,:,3) = AnnualDeathwoodroot./AnnualKtotroot;
        end

        if ii > startii; %For year 2 and beyond use what was calculated during previous year
            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD1hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'FreshCWD'
            oldfile.FreshCWD(isnan(oldfile.FreshCWD)) = 0;
            FreshCWD1hr(:,:,1:3) = oldfile.FreshCWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD10hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'FreshCWD'
            oldfile.FreshCWD(isnan(oldfile.FreshCWD)) = 0;
            FreshCWD10hr(:,:,1:3) = oldfile.FreshCWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD100hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'FreshCWD'
            oldfile.FreshCWD(isnan(oldfile.FreshCWD)) = 0;
            FreshCWD100hr(:,:,1:3) = oldfile.FreshCWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD1000hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'FreshCWD'
            oldfile.FreshCWD(isnan(oldfile.FreshCWD)) = 0;
            FreshCWD1000hr(:,:,1:3) = oldfile.FreshCWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWDtrunk_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'FreshCWD'
            oldfile.FreshCWD(isnan(oldfile.FreshCWD)) = 0;
            FreshCWDtrunk(:,:,1:3) = oldfile.FreshCWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWDroot_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'FreshCWD'
            oldfile.FreshCWD(isnan(oldfile.FreshCWD)) = 0;
            FreshCWDroot(:,:,1:3) = oldfile.FreshCWD(:,:,13:15);
        end

        %Now calculate budgets for each Fresh pool
        %function PoolTrackerFresh(Disturbclass, FreshCWD, Deathwood, KCWD, AgeKCWD, Disturbwood,Freshname, Decompfreshname, Disturbfreshname);
        %global Disturbfractremoval Freshslowdown outdirectoryroot ARDTile ii years

        PoolTrackerFresh(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 9, Disturbtype, Disturbfraction, FreshCWD1hr, Deathwood1hr, Kdecompwood1hrmonth, Kagewood1hrmonth, Disturbwood1hr,'_FreshCWD1hr_', '_DecompfreshCWD1hr_', '_AgingfreshCWD1hr_', '_DisturbremovalfreshCWD1hr_');
        PoolTrackerFresh(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 10, Disturbtype, Disturbfraction, FreshCWD10hr, Deathwood10hr, Kdecompwood10hrmonth, Kagewood10hrmonth, Disturbwood10hr,'_FreshCWD10hr_', '_DecompfreshCWD10hr_',  '_AgingfreshCWD10hr_', '_DisturbremovalfreshCWD10hr_');
        PoolTrackerFresh(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 11, Disturbtype, Disturbfraction, FreshCWD100hr, Deathwood100hr, Kdecompwood100hrmonth, Kagewood100hrmonth, Disturbwood100hr,'_FreshCWD100hr_', '_DecompfreshCWD100hr_',  '_AgingfreshCWD100hr_', '_DisturbremovalfreshCWD100hr_');
        PoolTrackerFresh(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 12, Disturbtype, Disturbfraction, FreshCWD1000hr, Deathwood1000hr, Kdecompwood1000hrmonth, Kagewood1000hrmonth, Disturbwood1000hr,'_FreshCWD1000hr_', '_DecompfreshCWD1000hr_',  '_AgingfreshCWD1000hr_', '_DisturbremovalfreshCWD1000hr_');
        PoolTrackerFresh(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 13, Disturbtype, Disturbfraction, FreshCWDtrunk, Deathwoodtrunk, Kdecompwoodtrunkmonth, Kagewoodtrunkmonth, Disturbwoodtrunk,'_FreshCWDtrunk_', '_DecompfreshCWDtrunk_',  '_AgingfreshCWDtrunk_', '_DisturbremovalfreshCWDtrunk_');
        PoolTrackerFresh(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 14, Disturbtype, Disturbfraction, FreshCWDroot, Deathwoodroot, Kdecompwoodrootmonth, Kagewoodrootmonth, Disturbwoodroot,'_FreshCWDroot_', '_DecompfreshCWDroot_',  '_AgingfreshCWDroot_', '_DisturbremovalfreshCWDroot_');

        %clear Deathwoodroot Disturbancewoodroot FreshCWDroot DecompfreshCWDroot DisturbremovalfreshCWDroot;
        %clearvars -except  Blocks2run SubTile2run SubTile K* ii startii ARDTile ARDTile indirectoryroot outdirectoryroot Beginyear Endyear years Years2run Months2run mnth TCWD1hr TCWD10hr TCWD100hr TCWD1000hr TCWDtrunk TCWDroot Tleafevrg Tleafherb Tfroot KCWD1hr KCWD10hr KCWD100hr KCWD1000hr KCWDtrunk KCWDroot Kleafevrg Kleafherb Kfroot Freshslowdown AgeTCWD1hr AgeTCWD10hr AgeTCWD100hr AgeTCWD1000hr AgeTCWDtrunk AgeTCWDroot AgeKCWD1hr AgeKCWD10hr AgeKCWD100hr AgeKCWD1000hr AgeKCWDtrunk AgeKCWDroot Disturbfractremoval Disturbfraction Disturbtype Disturbfractremoval Freshslowdown

        %Now we move on to the aged pools - start by reading in the aging flux just calculated above
        Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_AgingfreshCWD1hr_' num2str(years(ii)) '.mat']
        load(Inname); %AgingCWD
        AgingCWD1hr = AgingCWD;

        Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_AgingfreshCWD10hr_' num2str(years(ii)) '.mat']
        load(Inname); %AgingCWD
        AgingCWD10hr = AgingCWD;

        Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_AgingfreshCWD100hr_' num2str(years(ii)) '.mat']
        load(Inname); %AgingCWD
        AgingCWD100hr = AgingCWD;

        Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_AgingfreshCWD1000hr_' num2str(years(ii)) '.mat']
        load(Inname); %AgingCWD
        AgingCWD1000hr = AgingCWD;

        Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_AgingfreshCWDtrunk_' num2str(years(ii)) '.mat']
        load(Inname); %AgingCWD
        AgingCWDtrunk = AgingCWD;

        Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_AgingfreshCWDroot_' num2str(years(ii)) '.mat']
        load(Inname); %AgingCWD
        AgingCWDroot = AgingCWD;

        %Determinine initial pools -  start with analytical steady state calculation
        %Create pools structure and populate previous Dec (layer 3)
        CWD1hr = single(zeros(size(AgingCWD1hr)));
        CWD10hr = CWD1hr;
        CWD100hr = CWD1hr;
        CWD1000hr = CWD1hr;
        CWDtrunk = CWD1hr;
        CWDroot = CWD1hr;

        if ii == startii; %Year 1 initialize pool size based on input to pool/output K
            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD1hr_' num2str(years(ii)) '.mat']
            load(Inname); %,'FreshCWD'
            AgingCWD1hr = FreshCWD .* Kagewood1hrmonth; %Calc aging loss to non-fresh pool
            AnnualAgingwood1hr = nansum(AgingCWD1hr(:,:,4:15),3);
            CWD1hr(:,:,3) = AnnualAgingwood1hr./Kdecompwood1hryear;

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD10hr_' num2str(years(ii)) '.mat']
            load(Inname); %,'FreshCWD'
            AgingCWD10hr = FreshCWD .* Kagewood10hrmonth; %Calc aging loss to non-fresh pool
            AnnualAgingwood10hr = nansum(AgingCWD10hr(:,:,4:15),3);
            CWD10hr(:,:,3) = AnnualAgingwood10hr./Kdecompwood10hryear;

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD100hr_' num2str(years(ii)) '.mat']
            load(Inname); %,'FreshCWD'
            AgingCWD100hr = FreshCWD .* Kagewood100hrmonth; %Calc aging loss to non-fresh pool
            AnnualAgingwood100hr = nansum(AgingCWD100hr(:,:,4:15),3);
            CWD100hr(:,:,3) = AnnualAgingwood100hr./Kdecompwood100hryear;

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWD1000hr_' num2str(years(ii)) '.mat']
            load(Inname); %,'FreshCWD'
            AgingCWD1000hr = FreshCWD .* Kagewood1000hrmonth; %Calc aging loss to non-fresh pool
            AnnualAgingwood1000hr = nansum(AgingCWD1000hr(:,:,4:15),3);
            CWD1000hr(:,:,3) = AnnualAgingwood1000hr./Kdecompwood1000hryear;

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWDtrunk_' num2str(years(ii)) '.mat']
            load(Inname); %,'FreshCWD'
            AgingCWDtrunk = FreshCWD .* Kagewoodtrunkmonth; %Calc aging loss to non-fresh pool
            AnnualAgingwoodtrunk = nansum(AgingCWDtrunk(:,:,4:15),3);
            CWDtrunk(:,:,3) = AnnualAgingwoodtrunk./Kdecompwoodtrunkyear;

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_FreshCWDroot_' num2str(years(ii)) '.mat']
            load(Inname); %,'FreshCWD'
            AgingCWDroot = FreshCWD .* Kagewoodrootmonth; %Calc aging loss to non-fresh pool
            AnnualAgingwoodroot = nansum(AgingCWDroot(:,:,4:15),3);
            CWDroot(:,:,3) = AnnualAgingwoodroot./Kdecompwoodrootyear;
        end

        if ii > startii; %After yr 1 just use observations from previous year
            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_CWD1hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            oldfile.CWD(isnan(oldfile.CWD)) = 0;
            CWD1hr(:,:,1:3) = oldfile.CWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_CWD10hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            oldfile.CWD(isnan(oldfile.CWD)) = 0;
            CWD10hr(:,:,1:3) = oldfile.CWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_CWD100hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            oldfile.CWD(isnan(oldfile.CWD)) = 0;
            CWD100hr(:,:,1:3) = oldfile.CWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_CWD1000hr_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            oldfile.CWD(isnan(oldfile.CWD)) = 0;
            CWD1000hr(:,:,1:3) = oldfile.CWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_CWDtrunk_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            oldfile.CWD(isnan(oldfile.CWD)) = 0;
            CWDtrunk(:,:,1:3) = oldfile.CWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_CWDroot_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            oldfile.CWD(isnan(oldfile.CWD)) = 0;
            CWDroot(:,:,1:3) = oldfile.CWD(:,:,13:15);
        end

        %global Disturbfractremoval Freshslowdown outdirectoryroot ARDTile ii years
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 17, Disturbtype, Disturbfraction, CWD1hr, AgingCWD1hr, Kdecompwood1hrmonth, '_CWD1hr_', '_DecompCWD1hr_', '_DisturbremovalCWD1hr_');
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 18, Disturbtype, Disturbfraction, CWD10hr, AgingCWD10hr, Kdecompwood10hrmonth, '_CWD10hr_', '_DecompCWD10hr_', '_DisturbremovalCWD10hr_');
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 19, Disturbtype, Disturbfraction, CWD100hr, AgingCWD100hr, Kdecompwood100hrmonth, '_CWD100hr_', '_DecompCWD100hr_', '_DisturbremovalCWD100hr_');
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 20, Disturbtype, Disturbfraction, CWD1000hr, AgingCWD1000hr, Kdecompwood1000hrmonth, '_CWD1000hr_', '_DecompCWD1000hr_', '_DisturbremovalCWD1000hr_');
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 21, Disturbtype, Disturbfraction, CWDtrunk, AgingCWDtrunk, Kdecompwoodtrunkmonth, '_CWDtrunk_', '_DecompCWDtrunk_', '_DisturbremovalCWDtrunk_');
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 22, Disturbtype, Disturbfraction, CWDroot, AgingCWDroot, Kdecompwoodrootmonth, '_CWDroot_', '_DecompCWDroot_', '_DisturbremovalCWDroot_');

        %clearvars -except Blocks2run SubTile2run SubTile K* ii startii ARDTile ARDTile indirectoryroot outdirectoryroot Beginyear Endyear years Years2run Months2run mnth TCWD1hr TCWD10hr TCWD100hr TCWD1000hr TCWDtrunk TCWDroot Tleafevrg Tleafherb Tfroot KCWD1hr KCWD10hr KCWD100hr KCWD1000hr KCWDtrunk KCWDroot Kleafevrg Kleafherb Kfroot Freshslowdown AgeTCWD1hr AgeTCWD10hr AgeTCWD100hr AgeTCWD1000hr AgeTCWDtrunk AgeTCWDroot AgeKCWD1hr AgeKCWD10hr AgeKCWD100hr AgeKCWD1000hr AgeKCWDtrunk AgeKCWDroot Disturbfractremoval Disturbfraction Disturbtype Disturbfractremoval Freshslowdown

        %And finally, we do the leaf and fine root detrital pools
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtreeleaf_' num2str(years(ii)) '.mat']
        load(Inname); %Deattreeleaf
        Deathevrgleaf = Deathtreeleaf;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrubleaf_' num2str(years(ii)) '.mat']
        load(Inname); %Deatshrubleaf
        Deathevrgleaf = Deathevrgleaf + Deathshrubleaf;
        Inname = [indirectoryroot '/IntegrationModel/Block2/'  ARDTile '_Deathherbleaf_' num2str(years(ii)) '.mat']
        load(Inname); %Deathherbleaf

        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathtreefroot_' num2str(years(ii)) '.mat']
        load(Inname); %Deattreefroot
        Deathfroot = Deathtreefroot;
        Inname = [indirectoryroot '/IntegrationModel/Block2/' ARDTile '_Deathshrubfroot_' num2str(years(ii)) '.mat']
        load(Inname); %Deatshrubfroot
        Deathfroot = Deathfroot + Deathshrubfroot;
        Inname = [indirectoryroot '/IntegrationModel/Block2/'  ARDTile '_Deathherbroot_' num2str(years(ii)) '.mat']
        load(Inname); %Deathherbroot
        Deathfroot = Deathfroot  + Deathherbroot;

        %Determinine initial pools - start with analytical steady state calculation
        Litterevrgleaf = single(zeros(size(Deathherbroot)));
        Litterherbleaf = Litterevrgleaf;
        Litterfroot = Litterevrgleaf;

        if ii == startii; %For year 1 initialize pool size based on input to pool/output K
            AnnualDeathevrgleaf = nansum(Deathevrgleaf(:,:,4:15),3);
            AnnualKevrg = 1-(power(exp(-1), 1.0)); %Decomp term
            Litterevrgleaf(:,:,3)  = AnnualDeathevrgleaf./AnnualKevrg;

            AnnualDeathherbleaf = nansum(Deathherbleaf(:,:,4:15),3);
            AnnualKherb = 1-(power(exp(-2), 1.0)); %Decomp term
            Litterherbleaf(:,:,3)  = AnnualDeathherbleaf./AnnualKherb;

            AnnualDeathfroot = nansum(Deathfroot(:,:,4:15),3);
            AnnualDeathfroot = AnnualDeathfroot + nansum(Deathherbroot(:,:,4:15),3);
            AnnualKfroot = 1-(power(exp(-2), 1.0)); %Decomp term
            Litterfroot(:,:,3)  = AnnualDeathfroot./AnnualKfroot;
        end

        if ii > startii; %For year > 1 use pools from last year
            Inname = [outdirectoryroot '/IntegrationModel/Block3/'  ARDTile '_Litterevrgleaf_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            Litterevrgleaf(:,:,1:3) = oldfile.CWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_Litterherbleaf_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            Litterherbleaf(:,:,1:3) = oldfile.CWD(:,:,13:15);

            Inname = [outdirectoryroot '/IntegrationModel/Block3/' ARDTile '_Litterfroot_' num2str(years(ii-1)) '.mat']
            oldfile = load(Inname); %,'CWD'
            Litterfroot(:,:,1:3) = oldfile.CWD(:,:,13:15);
        end

        %And now we track the leaf pools
        %global Disturbfractremoval Freshslowdown outdirectoryroot ARDTile ii years
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 23, Disturbtype, Disturbfraction, Litterevrgleaf, Deathevrgleaf, Kdecompleafevrgmonth, '_Litterevrgleaf_', '_Decompevrgleaf_', '_Disturbremovalevrgleaf_');
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 24, Disturbtype, Disturbfraction, Litterherbleaf, Deathherbleaf, Kdecompleafherbmonth, '_Litterherbleaf_', '_Decompherbleaf_', '_Disturbremovalherbleaf_');
        PoolTrackerAged(Disturbfractremoval, Freshslowdown, outdirectoryroot, ARDTile, ii, years, 22, Disturbtype, Disturbfraction, Litterfroot, Deathfroot, Kdecompfrootmonth, '_Litterfroot_', '_Decompfroot_', '_Disturbremovalfroot_');

    end
    clearvars -except Blocks2run SubTile2run SubTile ii startii ARDTile ARDTile indirectoryroot outdirectoryroot Beginyear Endyear years Years2run Months2run mnth TCWD1hr TCWD10hr TCWD100hr TCWD1000hr TCWDtrunk TCWDroot Tleafevrg Tleafherb Tfroot KCWD1hr KCWD10hr KCWD100hr KCWD1000hr KCWDtrunk KCWDroot Kleafevrg Kleafherb Kfroot Freshslowdown AgeTCWD1hr AgeTCWD10hr AgeTCWD100hr AgeTCWD1000hr AgeTCWDtrunk AgeTCWDroot AgeKCWD1hr AgeKCWD10hr AgeKCWD100hr AgeKCWD1000hr AgeKCWDtrunk AgeKCWDroot Disturbfractremoval
end
end  %Unremark if calling from SLURM

