% Takes swcs and performs stats_tree, exports gstats to gstats.csv

% path to your TREES toolbox directory
TREES_PATH = '/home/mdm/code/TREES';      

addpath(genpath(TREES_PATH));
start_trees;
pathPrefix  = './swcs/';
swcsPath = strcat(pathPrefix, '*.swc');

swcs = dir(swcsPath);
swcFiles = cell(length(swcs), 1);
swcNames = cell(length(swcs), 1);


csvFile = fopen('gstats.csv', 'a+');
%=========ONLY WORKS ON LINUX================
[status, cmdout]= system('wc -l gstats.csv');
scanCell = textscan(cmdout,'%u %s');
lineCount = scanCell{1}
%=========ONLY WORKS ON LINUX================
tic;
% Store names, compute stats
for i = lineCount:length(swcs)
    if mod(i,100) == 0
        fprintf('Computed gstats for swc %d of %d...\n', i, length(swcs));
        toc;
        tic;
    end
        
    swcName = swcs(i,1).name(1:end-4);
    treePath = strcat(pathPrefix, swcName, '.swc');

    
    try
        load_tree(treePath);
        stats = stats_tree([],[],[],'-x');
    catch
        fprintf('Failed to gather stats, skipping file %s \n', swcName);
        stats = [];
        trees = [];
        continue;
    end
    
    
    sholl = sholl_tree(1, 1);
    radius = length(sholl);
    sholl = sholl_tree(1, radius/49);
    
    
    fprintf(csvFile, swcName);
    fprintf(csvFile, ',');
    
    gstatsFields = fieldnames(stats.gstats);
    gstatsLastField = gstatsFields{numel(gstatsFields)};
    
    for j = 1:numel(gstatsFields)
       currentField = stats.gstats.(gstatsFields{j});
       fprintf(csvFile, '%d', currentField);
       fprintf(csvFile, ',');
    end
    for j = 1:length(sholl)
        fprintf(csvFile, '%d', sholl(j));
        fprintf(csvFile, ',');
    end
    fprintf(csvFile, '\r\n');
    stats = [];
    trees = [];
end


fclose(csvFile);
fprintf('All finished!\n');
