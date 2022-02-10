#**********************************************************************  
#	*주의 사항
#		현재 스크립트 파일은 파일명만 출력되어 있습니다.
#		따라서, 저장된 추출 결과 파일의 경로를 'read.table' 또는 'read.fwf'에 추가하여야 합니다.
#	예) 다운로드 받은 폴더명 : C:\Download
#	  ※ 파일 경로 추가 : "[다운로드 받은 폴더명]\기업활동조사_기업활동조사(제공)_2019_20191201_92007.txt"
# 		read.table("C:\Download\기업활동조사_기업활동조사(제공)_2019_20191201_92007.txt", ~~~
#		또는
#		read.fwf("C:\Download\기업활동조사_기업활동조사(제공)_2019_20191201_92007.txt", ~~~
#
#		R 스크립트는 R 에서 파일 경로만 수정하시면 바로 실행(Ctrl+Alt+R)가능하며,
#		데이터셋 생성 후에 R 의 여러 가지 분석 기능을 사용할 수 있습니다.
#
#**********************************************************************

install.packages("dplyr")
library(dplyr)
library(tidyr)
library(ggplot2)

mdis2020 <- read.table("2020_연간화미적용자료_20220210_72875.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                 , "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character", "numeric", "character", "character", "numeric"
                                                                                                 , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                 , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "numeric", "character", "numeric", "numeric", "character", "character", "character", "character", "character", "character", "character", "character"
                                                                                                 , "character", "numeric"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

mdis$V1<- recode_factor(mdis$V1, '1'="서울")
mdis$V1<- recode_factor(mdis$V1, '2'="광역시")
mdis$V1<- recode_factor(mdis$V1, '3'="중소도시")
mdis$V1<- recode_factor(mdis$V1, '4'="읍면지역")
mdis$V2<- recode_factor(mdis$V2, '11'="서울")
mdis$V2<- recode_factor(mdis$V2, '21'="부산")
mdis$V2<- recode_factor(mdis$V2, '22'="대구")
mdis$V2<- recode_factor(mdis$V2, '23'="인천")
mdis$V2<- recode_factor(mdis$V2, '24'="광주")
mdis$V2<- recode_factor(mdis$V2, '25'="대전")
mdis$V2<- recode_factor(mdis$V2, '26'="울산")
mdis$V2<- recode_factor(mdis$V2, '29'="세종")
mdis$V2<- recode_factor(mdis$V2, '31'="경기")
mdis$V2<- recode_factor(mdis$V2, '32'="강원")
mdis$V2<- recode_factor(mdis$V2, '33'="충북")
mdis$V2<- recode_factor(mdis$V2, '34'="충남")
mdis$V2<- recode_factor(mdis$V2, '35'="전북")
mdis$V2<- recode_factor(mdis$V2, '36'="전남")
mdis$V2<- recode_factor(mdis$V2, '37'="경북")
mdis$V2<- recode_factor(mdis$V2, '38'="경남")
mdis$V2<- recode_factor(mdis$V2, '39'="제주")
mdis$V3<- recode_factor(mdis$V3, '1'="초등학교")
mdis$V3<- recode_factor(mdis$V3, '2'="중학교")
mdis$V3<- recode_factor(mdis$V3, '3'="일반고")
mdis$V3<- recode_factor(mdis$V3, '4'="특성화고")
mdis$V4<- recode_factor(mdis$V4, '1'="1차")
mdis$V4<- recode_factor(mdis$V4, '2'="2차")
mdis$V5<- recode_factor(mdis$V5, '1'="참여")
mdis$V5<- recode_factor(mdis$V5, '2'="미참여")
mdis$V16<- recode_factor(mdis$V16, '1'="보육")
mdis$V16<- recode_factor(mdis$V16, '2'="학교수업 보충 및 심화")
mdis$V16<- recode_factor(mdis$V16, '3'="진학준비(특목고,대입(논술·수능) 등 준비)")
mdis$V16<- recode_factor(mdis$V16, '4'="불안심리")
mdis$V16<- recode_factor(mdis$V16, '5'="선행학습")
mdis$V16<- recode_factor(mdis$V16, '6'="친구를 사귀기 위해서")
mdis$V16<- recode_factor(mdis$V16, '7'="기타")
mdis$V17<- recode_factor(mdis$V17, '1'="보육")
mdis$V17<- recode_factor(mdis$V17, '2'="학교수업 보충 및 심화")
mdis$V17<- recode_factor(mdis$V17, '3'="진학준비(특목고,대입(논술·수능) 등 준비)")
mdis$V17<- recode_factor(mdis$V17, '4'="불안심리")
mdis$V17<- recode_factor(mdis$V17, '5'="선행학습")
mdis$V17<- recode_factor(mdis$V17, '6'="친구를 사귀기 위해서")
mdis$V17<- recode_factor(mdis$V17, '7'="기타")
mdis$V19<- recode_factor(mdis$V19, '1'="보육")
mdis$V19<- recode_factor(mdis$V19, '2'="취미·교양·재능계발")
mdis$V19<- recode_factor(mdis$V19, '3'="학교수업 보충")
mdis$V19<- recode_factor(mdis$V19, '4'="진학준비(예술·체육 중·고 및 예체능계열 대학 입시 준비)")
mdis$V19<- recode_factor(mdis$V19, '5'="친구를 사귀기 위해서")
mdis$V19<- recode_factor(mdis$V19, '6'="기타")
mdis$V20<- recode_factor(mdis$V20, '1'="보육")
mdis$V20<- recode_factor(mdis$V20, '2'="취미·교양·재능계발")
mdis$V20<- recode_factor(mdis$V20, '3'="학교수업 보충")
mdis$V20<- recode_factor(mdis$V20, '4'="진학준비(예술·체육 중·고 및 예체능계열 대학 입시 준비)")
mdis$V20<- recode_factor(mdis$V20, '5'="친구를 사귀기 위해서")
mdis$V20<- recode_factor(mdis$V20, '6'="기타")
mdis$V48<- recode_factor(mdis$V48, '1'="참여")
mdis$V48<- recode_factor(mdis$V48, '2'="미참여")
mdis$V51<- recode_factor(mdis$V51, '1'="남학생")
mdis$V51<- recode_factor(mdis$V51, '2'="여학생")
mdis$V54<- recode_factor(mdis$V54, '1'="상위(10%)")
mdis$V54<- recode_factor(mdis$V54, '2'="중상위(11 ~ 30%)")
mdis$V54<- recode_factor(mdis$V54, '3'="중위(31 ~ 60%)")
mdis$V54<- recode_factor(mdis$V54, '4'="중하위(61 ~ 80%)")
mdis$V54<- recode_factor(mdis$V54, '5'="하위(81 ~ 100%)")
mdis$V55<- recode_factor(mdis$V55, '1'="20대")
mdis$V55<- recode_factor(mdis$V55, '2'="30대")
mdis$V55<- recode_factor(mdis$V55, '3'="40대")
mdis$V55<- recode_factor(mdis$V55, '4'="50대")
mdis$V55<- recode_factor(mdis$V55, '5'="60대 이상")
mdis$V56<- recode_factor(mdis$V56, '11'="초등학교 졸업")
mdis$V56<- recode_factor(mdis$V56, '12'="초등학교 수료")
mdis$V56<- recode_factor(mdis$V56, '13'="초등학교 재학")
mdis$V56<- recode_factor(mdis$V56, '14'="초등학교 중퇴")
mdis$V56<- recode_factor(mdis$V56, '15'="초등학교 휴학")
mdis$V56<- recode_factor(mdis$V56, '21'="중학교 졸업")
mdis$V56<- recode_factor(mdis$V56, '22'="중학교 수료")
mdis$V56<- recode_factor(mdis$V56, '23'="중학교 재학")
mdis$V56<- recode_factor(mdis$V56, '24'="중학교 중퇴")
mdis$V56<- recode_factor(mdis$V56, '25'="중학교 휴학")
mdis$V56<- recode_factor(mdis$V56, '31'="고등학교 졸업")
mdis$V56<- recode_factor(mdis$V56, '32'="고등학교 수료")
mdis$V56<- recode_factor(mdis$V56, '33'="고등학교 재학")
mdis$V56<- recode_factor(mdis$V56, '34'="고등학교 중퇴")
mdis$V56<- recode_factor(mdis$V56, '35'="고등학교 휴학")
mdis$V56<- recode_factor(mdis$V56, '41'="대학(4년제미만) 졸업")
mdis$V56<- recode_factor(mdis$V56, '42'="대학(4년제미만) 수료")
mdis$V56<- recode_factor(mdis$V56, '43'="대학(4년제미만) 재학")
mdis$V56<- recode_factor(mdis$V56, '44'="대학(4년제미만) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '45'="대학(4년제미만) 휴학")
mdis$V56<- recode_factor(mdis$V56, '51'="대학(4년제이상) 졸업")
mdis$V56<- recode_factor(mdis$V56, '52'="대학(4년제이상) 수료")
mdis$V56<- recode_factor(mdis$V56, '53'="대학(4년제이상) 재학")
mdis$V56<- recode_factor(mdis$V56, '54'="대학(4년제이상) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '55'="대학(4년제이상) 휴학")
mdis$V56<- recode_factor(mdis$V56, '61'="대학원(석사) 졸업")
mdis$V56<- recode_factor(mdis$V56, '62'="대학원(석사) 수료")
mdis$V56<- recode_factor(mdis$V56, '63'="대학원(석사) 재학")
mdis$V56<- recode_factor(mdis$V56, '64'="대학원(석사) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '65'="대학원(석사) 휴학")
mdis$V56<- recode_factor(mdis$V56, '71'="대학원(박사) 졸업")
mdis$V56<- recode_factor(mdis$V56, '72'="대학원(박사) 수료")
mdis$V56<- recode_factor(mdis$V56, '73'="대학원(박사) 재학")
mdis$V56<- recode_factor(mdis$V56, '74'="대학원(박사) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '75'="대학원(박사) 휴학")
mdis$V56<- recode_factor(mdis$V56, '8'="안받았음")
mdis$V57<- recode_factor(mdis$V57, '1'="20대")
mdis$V57<- recode_factor(mdis$V57, '2'="30대")
mdis$V57<- recode_factor(mdis$V57, '3'="40대")
mdis$V57<- recode_factor(mdis$V57, '4'="50대")
mdis$V57<- recode_factor(mdis$V57, '5'="60대 이상")
mdis$V58<- recode_factor(mdis$V58, '11'="초등학교 졸업")
mdis$V58<- recode_factor(mdis$V58, '12'="초등학교 수료")
mdis$V58<- recode_factor(mdis$V58, '13'="초등학교 재학")
mdis$V58<- recode_factor(mdis$V58, '14'="초등학교 중퇴")
mdis$V58<- recode_factor(mdis$V58, '15'="초등학교 휴학")
mdis$V58<- recode_factor(mdis$V58, '21'="중학교 졸업")
mdis$V58<- recode_factor(mdis$V58, '22'="중학교 수료")
mdis$V58<- recode_factor(mdis$V58, '23'="중학교 재학")
mdis$V58<- recode_factor(mdis$V58, '24'="중학교 중퇴")
mdis$V58<- recode_factor(mdis$V58, '25'="중학교 휴학")
mdis$V58<- recode_factor(mdis$V58, '31'="고등학교 졸업")
mdis$V58<- recode_factor(mdis$V58, '32'="고등학교 수료")
mdis$V58<- recode_factor(mdis$V58, '33'="고등학교 재학")
mdis$V58<- recode_factor(mdis$V58, '34'="고등학교 중퇴")
mdis$V58<- recode_factor(mdis$V58, '35'="고등학교 휴학")
mdis$V58<- recode_factor(mdis$V58, '41'="대학(4년제미만) 졸업")
mdis$V58<- recode_factor(mdis$V58, '42'="대학(4년제미만) 수료")
mdis$V58<- recode_factor(mdis$V58, '43'="대학(4년제미만) 재학")
mdis$V58<- recode_factor(mdis$V58, '44'="대학(4년제미만) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '45'="대학(4년제미만) 휴학")
mdis$V58<- recode_factor(mdis$V58, '51'="대학(4년제이상) 졸업")
mdis$V58<- recode_factor(mdis$V58, '52'="대학(4년제이상) 수료")
mdis$V58<- recode_factor(mdis$V58, '53'="대학(4년제이상) 재학")
mdis$V58<- recode_factor(mdis$V58, '54'="대학(4년제이상) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '55'="대학(4년제이상) 휴학")
mdis$V58<- recode_factor(mdis$V58, '61'="대학원(석사) 졸업")
mdis$V58<- recode_factor(mdis$V58, '62'="대학원(석사) 수료")
mdis$V58<- recode_factor(mdis$V58, '63'="대학원(석사) 재학")
mdis$V58<- recode_factor(mdis$V58, '64'="대학원(석사) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '65'="대학원(석사) 휴학")
mdis$V58<- recode_factor(mdis$V58, '71'="대학원(박사) 졸업")
mdis$V58<- recode_factor(mdis$V58, '72'="대학원(박사) 수료")
mdis$V58<- recode_factor(mdis$V58, '73'="대학원(박사) 재학")
mdis$V58<- recode_factor(mdis$V58, '74'="대학원(박사) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '75'="대학원(박사) 휴학")
mdis$V58<- recode_factor(mdis$V58, '8'="안받았음")
mdis$V59<- recode_factor(mdis$V59, '1'="부만 참여")
mdis$V59<- recode_factor(mdis$V59, '2'="모만 참여")
mdis$V59<- recode_factor(mdis$V59, '3'="부,모 모두 참여")
mdis$V59<- recode_factor(mdis$V59, '4'="부,모 모두 미참여")
mdis$V60<- recode_factor(mdis$V60, '1'="200만원 미만")
mdis$V60<- recode_factor(mdis$V60, '2'="200~300만원 미만")
mdis$V60<- recode_factor(mdis$V60, '3'="300~400만원 미만")
mdis$V60<- recode_factor(mdis$V60, '4'="400~500만원 미만")
mdis$V60<- recode_factor(mdis$V60, '5'="500~600만원 미만")
mdis$V60<- recode_factor(mdis$V60, '6'="600~700만원 미만")
mdis$V60<- recode_factor(mdis$V60, '7'="700~800만원 미만")
mdis$V60<- recode_factor(mdis$V60, '8'="800만원 이상")
mdis$V61<- recode_factor(mdis$V61, '1'="일반고(자율형공립고 포함)")
mdis$V61<- recode_factor(mdis$V61, '2'="자율형사립고")
mdis$V61<- recode_factor(mdis$V61, '3'="과학고, 영재학교")
mdis$V61<- recode_factor(mdis$V61, '4'="외고, 국제고")
mdis$V61<- recode_factor(mdis$V61, '5'="예술고, 체육고")
mdis$V61<- recode_factor(mdis$V61, '6'="마이스터고")
mdis$V61<- recode_factor(mdis$V61, '7'="특성화고")
mdis$V61<- recode_factor(mdis$V61, '8'="대안학교")
mdis$V61<- recode_factor(mdis$V61, '9'="해외유학")
mdis$V62<- recode_factor(mdis$V62, '1'="교육계열")
mdis$V62<- recode_factor(mdis$V62, '10'="서비스")
mdis$V62<- recode_factor(mdis$V62, '11'="기타(아직 결정 안함, 진학 안함 등)")
mdis$V62<- recode_factor(mdis$V62, '2'="예술 및 인문학")
mdis$V62<- recode_factor(mdis$V62, '3'="사회과학, 언론 및 정보학")
mdis$V62<- recode_factor(mdis$V62, '4'="경영, 행정 및 법")
mdis$V62<- recode_factor(mdis$V62, '5'="자연 과학, 수학 및 통계학")
mdis$V62<- recode_factor(mdis$V62, '6'="정보통신기술")
mdis$V62<- recode_factor(mdis$V62, '7'="공학, 제조 및 건설")
mdis$V62<- recode_factor(mdis$V62, '8'="농림어업 및 수의학")
mdis$V62<- recode_factor(mdis$V62, '9'="보건 및 복지")

colnames(mdis2020) = c("지역구분코드"
                       , "행정구역시도코드", "학교급구분코드", "조사차시", "방과후학교참여여부", "방과후학교참여시간수", "방과후학교총비용", "방과후학교_초등방과후보육프로그램비용", "방과후학교_특기적성프로그램비용", "방과후학교_교과프로그램비용", "EBS교재비", "어학연수비용", "국내연수비용", "해외연수비용", "사교육참여시간수", "일반교과사교육목적구분1코드", "일반교과사교육목적구분2코드", "일반교과사교육시간수", "예체능사교육목적구분1코드", "예체능사교육목적구분2코드", "예체능사교육시간수"
                       , "취업관련사교육시간수", "사교육비총비용", "사교육비_일반교과비용", "사교육비_일반교과_국어비용", "사교육비_일반교과_영어비용", "사교육비_일반교과_수학비용", "사교육비_일반교과_사회과학비용", "사교육비_일반교과_논술비용", "사교육비_일반교과_컴퓨터비용", "사교육비_일반교과_제2외국어한문컴퓨터일반기술가정비용", "사교육비_일반교과_개인과외비용", "사교육비_일반교과_그룹과외비용", "사교육비_일반교과_학원수강비용", "사교육비_일반교과_방문학습지비용", "사교육비_일반교과_유료인터넷및통신강좌비용", "사교육비_일반교과_기타비용", "사교육비_예체능취미교양비용", "사교육비_예체능취미교양_음악비용", "사교육비_예체능취미교양_미술비용", "사교육비_예체능취미교양_체육비용"
                       , "사교육비_예체능취미교양_취미교양비용", "사교육비_예체능취미교양_개인과외비용", "사교육비_예체능취미교양_그룹과외비용", "사교육비_예체능취미교양_학원수강비용", "사교육비_예체능취미교양_방문수업및기타비용", "취업관련비용", "진로진학학습상담컨설팅참여여부", "진로진학학습상담컨설팅비용", "진로진학학습상담컨설팅횟수", "학생성별코드", "총자녀수", "출생순위", "학생성적구분코드", "부연령코드", "부교육정도코드", "모연령코드", "모교육정도코드", "부모경제활동코드", "월평균가구소득코드", "진학희망고등학교유형코드"
                       , "진학희망대학전공영역코드", "가중값")

mdis2019 <- read.table("2019_연간화미적용자료_20220210_72875.csv", header=FALSE, sep=",", colClasses = c("character"
                                                                                                 , "character", "character", "character", "character", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character", "numeric", "character", "character", "numeric"
                                                                                                 , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"
                                                                                                 , "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "numeric", "numeric", "character", "numeric", "numeric", "character", "character", "character", "character", "character", "character", "character", "character"
                                                                                                 , "character", "numeric"), skip=1, na.string=c("*","**","***","****","*****","******","*******","********","*********","**********","."))

mdis$V1<- recode_factor(mdis$V1, '1'="서울")
mdis$V1<- recode_factor(mdis$V1, '2'="광역시")
mdis$V1<- recode_factor(mdis$V1, '3'="중소도시")
mdis$V1<- recode_factor(mdis$V1, '4'="읍면지역")
mdis$V2<- recode_factor(mdis$V2, '11'="서울")
mdis$V2<- recode_factor(mdis$V2, '21'="부산")
mdis$V2<- recode_factor(mdis$V2, '22'="대구")
mdis$V2<- recode_factor(mdis$V2, '23'="인천")
mdis$V2<- recode_factor(mdis$V2, '24'="광주")
mdis$V2<- recode_factor(mdis$V2, '25'="대전")
mdis$V2<- recode_factor(mdis$V2, '26'="울산")
mdis$V2<- recode_factor(mdis$V2, '29'="세종")
mdis$V2<- recode_factor(mdis$V2, '31'="경기")
mdis$V2<- recode_factor(mdis$V2, '32'="강원")
mdis$V2<- recode_factor(mdis$V2, '33'="충북")
mdis$V2<- recode_factor(mdis$V2, '34'="충남")
mdis$V2<- recode_factor(mdis$V2, '35'="전북")
mdis$V2<- recode_factor(mdis$V2, '36'="전남")
mdis$V2<- recode_factor(mdis$V2, '37'="경북")
mdis$V2<- recode_factor(mdis$V2, '38'="경남")
mdis$V2<- recode_factor(mdis$V2, '39'="제주")
mdis$V3<- recode_factor(mdis$V3, '1'="초등학교")
mdis$V3<- recode_factor(mdis$V3, '2'="중학교")
mdis$V3<- recode_factor(mdis$V3, '3'="일반고")
mdis$V3<- recode_factor(mdis$V3, '4'="특성화고")
mdis$V4<- recode_factor(mdis$V4, '1'="1차")
mdis$V4<- recode_factor(mdis$V4, '2'="2차")
mdis$V5<- recode_factor(mdis$V5, '1'="참여")
mdis$V5<- recode_factor(mdis$V5, '2'="미참여")
mdis$V16<- recode_factor(mdis$V16, '1'="보육")
mdis$V16<- recode_factor(mdis$V16, '2'="학교수업 보충 및 심화")
mdis$V16<- recode_factor(mdis$V16, '3'="진학준비(특목고,대입(논술·수능) 등 준비)")
mdis$V16<- recode_factor(mdis$V16, '4'="불안심리")
mdis$V16<- recode_factor(mdis$V16, '5'="선행학습")
mdis$V16<- recode_factor(mdis$V16, '6'="친구를 사귀기 위해서")
mdis$V16<- recode_factor(mdis$V16, '7'="기타")
mdis$V17<- recode_factor(mdis$V17, '1'="보육")
mdis$V17<- recode_factor(mdis$V17, '2'="학교수업 보충 및 심화")
mdis$V17<- recode_factor(mdis$V17, '3'="진학준비(특목고,대입(논술·수능) 등 준비)")
mdis$V17<- recode_factor(mdis$V17, '4'="불안심리")
mdis$V17<- recode_factor(mdis$V17, '5'="선행학습")
mdis$V17<- recode_factor(mdis$V17, '6'="친구를 사귀기 위해서")
mdis$V17<- recode_factor(mdis$V17, '7'="기타")
mdis$V19<- recode_factor(mdis$V19, '1'="보육")
mdis$V19<- recode_factor(mdis$V19, '2'="취미·교양·재능계발")
mdis$V19<- recode_factor(mdis$V19, '3'="학교수업 보충")
mdis$V19<- recode_factor(mdis$V19, '4'="진학준비(예술·체육 중·고 및 예체능계열 대학 입시 준비)")
mdis$V19<- recode_factor(mdis$V19, '5'="친구를 사귀기 위해서")
mdis$V19<- recode_factor(mdis$V19, '6'="기타")
mdis$V20<- recode_factor(mdis$V20, '1'="보육")
mdis$V20<- recode_factor(mdis$V20, '2'="취미·교양·재능계발")
mdis$V20<- recode_factor(mdis$V20, '3'="학교수업 보충")
mdis$V20<- recode_factor(mdis$V20, '4'="진학준비(예술·체육 중·고 및 예체능계열 대학 입시 준비)")
mdis$V20<- recode_factor(mdis$V20, '5'="친구를 사귀기 위해서")
mdis$V20<- recode_factor(mdis$V20, '6'="기타")
mdis$V48<- recode_factor(mdis$V48, '1'="참여")
mdis$V48<- recode_factor(mdis$V48, '2'="미참여")
mdis$V51<- recode_factor(mdis$V51, '1'="남학생")
mdis$V51<- recode_factor(mdis$V51, '2'="여학생")
mdis$V54<- recode_factor(mdis$V54, '1'="상위(10%)")
mdis$V54<- recode_factor(mdis$V54, '2'="중상위(11 ~ 30%)")
mdis$V54<- recode_factor(mdis$V54, '3'="중위(31 ~ 60%)")
mdis$V54<- recode_factor(mdis$V54, '4'="중하위(61 ~ 80%)")
mdis$V54<- recode_factor(mdis$V54, '5'="하위(81 ~ 100%)")
mdis$V55<- recode_factor(mdis$V55, '1'="20대")
mdis$V55<- recode_factor(mdis$V55, '2'="30대")
mdis$V55<- recode_factor(mdis$V55, '3'="40대")
mdis$V55<- recode_factor(mdis$V55, '4'="50대")
mdis$V55<- recode_factor(mdis$V55, '5'="60대 이상")
mdis$V56<- recode_factor(mdis$V56, '11'="초등학교 졸업")
mdis$V56<- recode_factor(mdis$V56, '12'="초등학교 수료")
mdis$V56<- recode_factor(mdis$V56, '13'="초등학교 재학")
mdis$V56<- recode_factor(mdis$V56, '14'="초등학교 중퇴")
mdis$V56<- recode_factor(mdis$V56, '15'="초등학교 휴학")
mdis$V56<- recode_factor(mdis$V56, '21'="중학교 졸업")
mdis$V56<- recode_factor(mdis$V56, '22'="중학교 수료")
mdis$V56<- recode_factor(mdis$V56, '23'="중학교 재학")
mdis$V56<- recode_factor(mdis$V56, '24'="중학교 중퇴")
mdis$V56<- recode_factor(mdis$V56, '25'="중학교 휴학")
mdis$V56<- recode_factor(mdis$V56, '31'="고등학교 졸업")
mdis$V56<- recode_factor(mdis$V56, '32'="고등학교 수료")
mdis$V56<- recode_factor(mdis$V56, '33'="고등학교 재학")
mdis$V56<- recode_factor(mdis$V56, '34'="고등학교 중퇴")
mdis$V56<- recode_factor(mdis$V56, '35'="고등학교 휴학")
mdis$V56<- recode_factor(mdis$V56, '41'="대학(4년제미만) 졸업")
mdis$V56<- recode_factor(mdis$V56, '42'="대학(4년제미만) 수료")
mdis$V56<- recode_factor(mdis$V56, '43'="대학(4년제미만) 재학")
mdis$V56<- recode_factor(mdis$V56, '44'="대학(4년제미만) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '45'="대학(4년제미만) 휴학")
mdis$V56<- recode_factor(mdis$V56, '51'="대학(4년제이상) 졸업")
mdis$V56<- recode_factor(mdis$V56, '52'="대학(4년제이상) 수료")
mdis$V56<- recode_factor(mdis$V56, '53'="대학(4년제이상) 재학")
mdis$V56<- recode_factor(mdis$V56, '54'="대학(4년제이상) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '55'="대학(4년제이상) 휴학")
mdis$V56<- recode_factor(mdis$V56, '61'="대학원(석사) 졸업")
mdis$V56<- recode_factor(mdis$V56, '62'="대학원(석사) 수료")
mdis$V56<- recode_factor(mdis$V56, '63'="대학원(석사) 재학")
mdis$V56<- recode_factor(mdis$V56, '64'="대학원(석사) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '65'="대학원(석사) 휴학")
mdis$V56<- recode_factor(mdis$V56, '71'="대학원(박사) 졸업")
mdis$V56<- recode_factor(mdis$V56, '72'="대학원(박사) 수료")
mdis$V56<- recode_factor(mdis$V56, '73'="대학원(박사) 재학")
mdis$V56<- recode_factor(mdis$V56, '74'="대학원(박사) 중퇴")
mdis$V56<- recode_factor(mdis$V56, '75'="대학원(박사) 휴학")
mdis$V56<- recode_factor(mdis$V56, '8'="안받았음")
mdis$V57<- recode_factor(mdis$V57, '1'="20대")
mdis$V57<- recode_factor(mdis$V57, '2'="30대")
mdis$V57<- recode_factor(mdis$V57, '3'="40대")
mdis$V57<- recode_factor(mdis$V57, '4'="50대")
mdis$V57<- recode_factor(mdis$V57, '5'="60대 이상")
mdis$V58<- recode_factor(mdis$V58, '11'="초등학교 졸업")
mdis$V58<- recode_factor(mdis$V58, '12'="초등학교 수료")
mdis$V58<- recode_factor(mdis$V58, '13'="초등학교 재학")
mdis$V58<- recode_factor(mdis$V58, '14'="초등학교 중퇴")
mdis$V58<- recode_factor(mdis$V58, '15'="초등학교 휴학")
mdis$V58<- recode_factor(mdis$V58, '21'="중학교 졸업")
mdis$V58<- recode_factor(mdis$V58, '22'="중학교 수료")
mdis$V58<- recode_factor(mdis$V58, '23'="중학교 재학")
mdis$V58<- recode_factor(mdis$V58, '24'="중학교 중퇴")
mdis$V58<- recode_factor(mdis$V58, '25'="중학교 휴학")
mdis$V58<- recode_factor(mdis$V58, '31'="고등학교 졸업")
mdis$V58<- recode_factor(mdis$V58, '32'="고등학교 수료")
mdis$V58<- recode_factor(mdis$V58, '33'="고등학교 재학")
mdis$V58<- recode_factor(mdis$V58, '34'="고등학교 중퇴")
mdis$V58<- recode_factor(mdis$V58, '35'="고등학교 휴학")
mdis$V58<- recode_factor(mdis$V58, '41'="대학(4년제미만) 졸업")
mdis$V58<- recode_factor(mdis$V58, '42'="대학(4년제미만) 수료")
mdis$V58<- recode_factor(mdis$V58, '43'="대학(4년제미만) 재학")
mdis$V58<- recode_factor(mdis$V58, '44'="대학(4년제미만) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '45'="대학(4년제미만) 휴학")
mdis$V58<- recode_factor(mdis$V58, '51'="대학(4년제이상) 졸업")
mdis$V58<- recode_factor(mdis$V58, '52'="대학(4년제이상) 수료")
mdis$V58<- recode_factor(mdis$V58, '53'="대학(4년제이상) 재학")
mdis$V58<- recode_factor(mdis$V58, '54'="대학(4년제이상) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '55'="대학(4년제이상) 휴학")
mdis$V58<- recode_factor(mdis$V58, '61'="대학원(석사) 졸업")
mdis$V58<- recode_factor(mdis$V58, '62'="대학원(석사) 수료")
mdis$V58<- recode_factor(mdis$V58, '63'="대학원(석사) 재학")
mdis$V58<- recode_factor(mdis$V58, '64'="대학원(석사) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '65'="대학원(석사) 휴학")
mdis$V58<- recode_factor(mdis$V58, '71'="대학원(박사) 졸업")
mdis$V58<- recode_factor(mdis$V58, '72'="대학원(박사) 수료")
mdis$V58<- recode_factor(mdis$V58, '73'="대학원(박사) 재학")
mdis$V58<- recode_factor(mdis$V58, '74'="대학원(박사) 중퇴")
mdis$V58<- recode_factor(mdis$V58, '75'="대학원(박사) 휴학")
mdis$V58<- recode_factor(mdis$V58, '8'="안받았음")
mdis$V59<- recode_factor(mdis$V59, '1'="부만 참여")
mdis$V59<- recode_factor(mdis$V59, '2'="모만 참여")
mdis$V59<- recode_factor(mdis$V59, '3'="부,모 모두 참여")
mdis$V59<- recode_factor(mdis$V59, '4'="부,모 모두 미참여")
mdis$V60<- recode_factor(mdis$V60, '1'="200만원 미만")
mdis$V60<- recode_factor(mdis$V60, '2'="200~300만원 미만")
mdis$V60<- recode_factor(mdis$V60, '3'="300~400만원 미만")
mdis$V60<- recode_factor(mdis$V60, '4'="400~500만원 미만")
mdis$V60<- recode_factor(mdis$V60, '5'="500~600만원 미만")
mdis$V60<- recode_factor(mdis$V60, '6'="600~700만원 미만")
mdis$V60<- recode_factor(mdis$V60, '7'="700~800만원 미만")
mdis$V60<- recode_factor(mdis$V60, '8'="800만원 이상")
mdis$V61<- recode_factor(mdis$V61, '1'="일반고(자율형공립고 포함)")
mdis$V61<- recode_factor(mdis$V61, '2'="자율형사립고")
mdis$V61<- recode_factor(mdis$V61, '3'="과학고, 영재학교")
mdis$V61<- recode_factor(mdis$V61, '4'="외고, 국제고")
mdis$V61<- recode_factor(mdis$V61, '5'="예술고, 체육고")
mdis$V61<- recode_factor(mdis$V61, '6'="마이스터고")
mdis$V61<- recode_factor(mdis$V61, '7'="특성화고")
mdis$V61<- recode_factor(mdis$V61, '8'="대안학교")
mdis$V61<- recode_factor(mdis$V61, '9'="해외유학")
mdis$V62<- recode_factor(mdis$V62, '1'="교육계열")
mdis$V62<- recode_factor(mdis$V62, '10'="서비스")
mdis$V62<- recode_factor(mdis$V62, '11'="기타(아직 결정 안함, 진학 안함 등)")
mdis$V62<- recode_factor(mdis$V62, '2'="예술 및 인문학")
mdis$V62<- recode_factor(mdis$V62, '3'="사회과학, 언론 및 정보학")
mdis$V62<- recode_factor(mdis$V62, '4'="경영, 행정 및 법")
mdis$V62<- recode_factor(mdis$V62, '5'="자연 과학, 수학 및 통계학")
mdis$V62<- recode_factor(mdis$V62, '6'="정보통신기술")
mdis$V62<- recode_factor(mdis$V62, '7'="공학, 제조 및 건설")
mdis$V62<- recode_factor(mdis$V62, '8'="농림어업 및 수의학")
mdis$V62<- recode_factor(mdis$V62, '9'="보건 및 복지")

colnames(mdis2019) = c("지역구분코드"
                       , "행정구역시도코드", "학교급구분코드", "조사차시", "방과후학교참여여부", "방과후학교참여시간수", "방과후학교총비용", "방과후학교_초등방과후보육프로그램비용", "방과후학교_특기적성프로그램비용", "방과후학교_교과프로그램비용", "EBS교재비", "어학연수비용", "국내연수비용", "해외연수비용", "사교육참여시간수", "일반교과사교육목적구분1코드", "일반교과사교육목적구분2코드", "일반교과사교육시간수", "예체능사교육목적구분1코드", "예체능사교육목적구분2코드", "예체능사교육시간수"
                       , "취업관련사교육시간수", "사교육비총비용", "사교육비_일반교과비용", "사교육비_일반교과_국어비용", "사교육비_일반교과_영어비용", "사교육비_일반교과_수학비용", "사교육비_일반교과_사회과학비용", "사교육비_일반교과_논술비용", "사교육비_일반교과_컴퓨터비용", "사교육비_일반교과_제2외국어한문컴퓨터일반기술가정비용", "사교육비_일반교과_개인과외비용", "사교육비_일반교과_그룹과외비용", "사교육비_일반교과_학원수강비용", "사교육비_일반교과_방문학습지비용", "사교육비_일반교과_유료인터넷및통신강좌비용", "사교육비_일반교과_기타비용", "사교육비_예체능취미교양비용", "사교육비_예체능취미교양_음악비용", "사교육비_예체능취미교양_미술비용", "사교육비_예체능취미교양_체육비용"
                       , "사교육비_예체능취미교양_취미교양비용", "사교육비_예체능취미교양_개인과외비용", "사교육비_예체능취미교양_그룹과외비용", "사교육비_예체능취미교양_학원수강비용", "사교육비_예체능취미교양_방문수업및기타비용", "취업관련비용", "진로진학학습상담컨설팅참여여부", "진로진학학습상담컨설팅비용", "진로진학학습상담컨설팅횟수", "학생성별코드", "총자녀수", "출생순위", "학생성적구분코드", "부연령코드", "부교육정도코드", "모연령코드", "모교육정도코드", "부모경제활동코드", "월평균가구소득코드", "진학희망고등학교유형코드"
                       , "진학희망대학전공영역코드", "가중값")


raw2020 <- mdis2020 %>%
  filter(학교급구분코드 == c(3,4)) %>%
  group_by(행정구역시도코드) %>%
  mutate(행정구역시도코드 = case_when(행정구역시도코드 == 11 ~ "서울",
                                      행정구역시도코드 == 21 ~ "부산",
                                      행정구역시도코드 == 22 ~ "대구",
                                      행정구역시도코드 == 23 ~ "인천",
                                      행정구역시도코드 == 24 ~ "광주",
                                      행정구역시도코드 == 25 ~ "대전",
                                      행정구역시도코드 == 26 ~ "울산", 
                                      행정구역시도코드 == 29 ~ "세종",
                                      행정구역시도코드 == 31 ~ "경기",
                                      행정구역시도코드 == 32 ~ "강원",
                                      행정구역시도코드 == 33 ~ "충북",
                                      행정구역시도코드 == 34 ~ "충남",
                                      행정구역시도코드 == 35 ~ "전북",
                                      행정구역시도코드 == 36 ~ "전남",
                                      행정구역시도코드 == 37 ~ "경북",
                                      행정구역시도코드 == 38 ~ "경남",
                                      행정구역시도코드 == 39 ~ "제주")) %>%
  summarise(총원   = n(),
              중위권 = length(which(학생성적구분코드드==3))/총원,
              중산층 = length(which(월평균가구소득코드==5))/총원,
              평균_교육비 = mean(사교육비총비용),
              평균_사교육시간 = mean(사교육참여시간수),
              평균_교과사교육시간수 = mean(일반교과사교육시간수),
              yr = factor(2020))

raw2019 <- mdis2019 %>%
  filter(학교급구분코드 == c(3,4)) %>%
  group_by(행정구역시도코드) %>%
  mutate(행정구역시도코드 = case_when(행정구역시도코드 == 11 ~ "서울",
                                      행정구역시도코드 == 21 ~ "부산",
                                      행정구역시도코드 == 22 ~ "대구",
                                      행정구역시도코드 == 23 ~ "인천",
                                      행정구역시도코드 == 24 ~ "광주",
                                      행정구역시도코드 == 25 ~ "대전",
                                      행정구역시도코드 == 26 ~ "울산", 
                                      행정구역시도코드 == 29 ~ "세종",
                                      행정구역시도코드 == 31 ~ "경기",
                                      행정구역시도코드 == 32 ~ "강원",
                                      행정구역시도코드 == 33 ~ "충북",
                                      행정구역시도코드 == 34 ~ "충남",
                                      행정구역시도코드 == 35 ~ "전북",
                                      행정구역시도코드 == 36 ~ "전남",
                                      행정구역시도코드 == 37 ~ "경북",
                                      행정구역시도코드 == 38 ~ "경남",
                                      행정구역시도코드 == 39 ~ "제주")) %>%
  summarise(총원   = n(),
              중위권 = length(which(학생성적구분코드드==3))/총원,
              중산층 = length(which(월평균가구소득코드==5))/총원,
              평균_교육비 = mean(사교육비총비용),
              평균_사교육시간 = mean(사교육참여시간수),
              평균_교과사교육시간수 = mean(일반교과사교육시간수),
              yr = factor(2019))

tempt <-  rbind(raw2020, raw2019) %>%
  filter(!행정구역시도코드 %in% c("경기", "강원", "충북", "충남", "전북", "전남", "경북", "경남"))

tempt$yr <- tempt$yr %>%
  factor(levels = c(2019, 2020))

tempt %>%
  ggplot(aes(x = yr, y = 중위권,
             
             colour = 행정구역시도코드)) +
  geom_line(aes(group = 행정구역시도코드)) +
  geom_point() +
  geom_text(aes(label = 행정구역시도코드),
            nudge_x = 0.003,
            nudge_y = 0.002 )

tempt %>%
  ggplot(aes(x = yr, y = 중산층,
             
             colour = 행정구역시도코드)) +
  geom_line(aes(group = 행정구역시도코드)) +
  geom_point() +
  geom_text(aes(label = 행정구역시도코드),
            nudge_x = 0.003,
            nudge_y = 0.002 )



tempt %>%
  ggplot(aes(x = yr, y = 평균_교육비,
             
             colour = 행정구역시도코드)) +
  geom_line(aes(group = 행정구역시도코드)) +
  geom_point() +
  geom_text(aes(label = 행정구역시도코드),
            nudge_x = 0.003,
            nudge_y = 0.002 )

tempt %>%
  ggplot(aes(x = yr, y = 평균_사교육시간,
             
             colour = 행정구역시도코드)) +
  geom_line(aes(group = 행정구역시도코드)) +
  geom_point() +
  geom_text(aes(label = 행정구역시도코드),
            nudge_x = 0.003,
            nudge_y = 0.002 )


