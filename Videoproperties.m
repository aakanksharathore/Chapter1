%% Code to calculate NND and contract for manually selected animals (click on all visible animals)
%%Written by AA on 1st Jan 2019

clear all;
%%% Read video
[videofile,pathname]=uigetfile({'*.avi';'*.MOV';'*.mp4'},'path','/home/blackbug');
vidread=VideoReader(fullfile(pathname,videofile));
nDur=vidread.Duration;

%vout = VideoWriter('~/herd.avi'); %Output file
%% Read frames
initial_frame = 1;           % starting frame
%endframe = nframe -10; 
nframe=round(nDur*vidread.FrameRate);
steps=500; % Number of frames to skip 
%vidread=VideoReader(videofile);

%% variables
df = cell2table(cell(0,4), 'VariableNames',{'c_id', 'x_px', 'y_px', 'fTime'});
nsteps=round(nDur*vidread.FrameRate/steps);
vidread.CurrentTime = 0;
fn=zeros(size(readFrame(vidread)));
X=cell(1,(nframe));
Y=cell(1,(nframe));
i=1;
j=0;
k=1;
l=1;
tempT=0;
%open(vout);
ANN = zeros(30,2);
rgb=zeros(30,4);
%%
 vidread.CurrentTime=0;
 %for text display
start_x = 30 ;
start_y = 20;
flag = 0;
for i=1:(round(nframe/steps)-10)
 if flag == 0
  j=j+steps;
  vidread.CurrentTime = vidread.CurrentTime+(1/vidread.FrameRate)*steps;
    if vidread.CurrentTime>=round(nDur-1)
        break;
    end
    fr = readFrame(vidread);
    Ir = fr(:,:,1);
    Ig = fr(:,:,2);
    Ib = fr(:,:,3);
    ir=mean(reshape(Ir,1,[]));
    ig=mean(reshape(Ig,1,[]));
    ib=mean(reshape(Ib,1,[]));
    %fr=rgb2gray(fr);
%% Motion detection
tmp=vidread.CurrentTime;
 if i < (round(nframe/steps)-50)
     
    vidread.CurrentTime = vidread.CurrentTime+(1/vidread.FrameRate)*(30);
    fr1 = readFrame(vidread);
 else
        
        vidread.CurrentTime = vidread.CurrentTime-(1/vidread.FrameRate)*(30);
    fr1 = readFrame(vidread);
 end
 vidread.CurrentTime=tmp;  
    
%%

 %fr1 = rgb2gray(fr1);
 
 %% Display focal frame
 text_str=['frame no. =  ', int2str(j)];
  im1=['Image',int2str(j),'.jpg'];
 im2=['Image',int2str(j),'_30.jpg'];
imwrite(fr,im1,'jpg');
imwrite(fr1,im2,'jpg');

 imshow(fr)
 hold on;
 text(start_x,start_y,text_str, 'Color', 'r','FontSize',14);
 [coordx, coordy] = getpts;
 if length(coordx) ~= 0
 [idx,d] = knnsearch([coordx,coordy],[coordx,coordy], 'k', 2);
 ANN(k,1)= j;
 ANN(k,2) = mean(d(:,2));
 k = k+1;
 end
 end

w = waitforbuttonpress;
%pause;
keyP=get(gcf,'CurrentCharacter');
if keyP == 'n'
    flag = 0;
    continue
elseif keyP == 'c'
     flag = 1;
     image(fr)
     hold on;
     text(start_x,start_y,text_str, 'Color', 'r','FontSize',14);
     [col,row] = ginput(1);
     rgb(l,1:3)=[abs(double(fr(round(row,0),round(col,0),1))-ir),abs(double(fr(round(row,0),round(col,0),2))-ig),abs(double(fr(round(row,0),round(col,0),3))-ib)];
     rgb(l,4)=j;
     l=l+1;
elseif keyP == 'q'
    break;
%elseif keyP == 's'
 %ginput();
% [nndx, nndy] = [sqrt(mean(X^2)), sqrt(mean(Y^2))];

end

end
hold off;



