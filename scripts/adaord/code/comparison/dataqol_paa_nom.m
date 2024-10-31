%It needs the functions for PAA in https://github.com/aalab/naa/

Xd=dlmread('Xd.txt');

options = generate_options(); 
options.verbose = true; 
options.display = false;
options.eps = 10^-6;
options.maxIter = 1000;
options.matFeatLat = [];



nFeat = 28; % number of features (views)

cate=repmat(4,28,1);    
cate(5)=2;
cate(24)=3;

i=1;
% Preparing data for paa, one cell entry for each view
nFeatSam = cell(nFeat, 1);
for count = 1:nFeat
    % each column has one 1 and rest are 0s
		j= i +cate(count) -1;
    nFeatSam{count} = Xd(:, i:j)';
		i=j+1;
end


num= 4 %20;

rng(1234)

% Learning archetypes using VB, maximum number of archetypes
options.priorMatLatSam = 0.3;
options.priorMatSamLat = 0.1;
[matSamLat_VB, matLatSam_VB, ~] = paa_nominal_VB(nFeatSam, num, options);
matSamLat_VB = bsxfun(@rdivide, matSamLat_VB, sum(matSamLat_VB));
matLatSam_VB = bsxfun(@rdivide, matLatSam_VB, sum(matLatSam_VB));

% Finding active archetypes
activeArchetypes = find(max(matLatSam_VB, [], 2) > 0.15);
fprintf('number of active archetypes %d\n', length(activeArchetypes))
matSamLat_VB = matSamLat_VB(:, activeArchetypes);
matLatSam_VB = matLatSam_VB(activeArchetypes, :);
matLatSam_VB = bsxfun(@rdivide, matLatSam_VB, sum(matLatSam_VB));

% Archetypes for each view
archetypes_VB = cell(nFeat, 1);
for count = 1:nFeat
    archetypes_VB{count} = nFeatSam{count} * matSamLat_VB;
end

%threshold
th=0.8;
aar=zeros(4,28);
for i=1:28

[m,I]=max(archetypes_VB{i});
 aar(find(m>th),i) = I(m>th);
end

%%%maximum
aar=[];
for i=1:28

[m,I]=max(archetypes_VB{i});
aar=[aar I'];
end


