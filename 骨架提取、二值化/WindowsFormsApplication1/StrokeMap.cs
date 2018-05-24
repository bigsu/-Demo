
using System;
using System.Collections.Generic;
using System.ComponentModel;
using System.Data;
using System.Drawing;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Windows.Forms;
using System.Drawing.Drawing2D;
using System.IO;

namespace StrokeGen
{
    //Total 32 Strokes
    //Map each Chinese Character to different Stroken Names

    //Map to 5x5 Grid



    public class StrokeMap
    {
        //当前汉字索引。
        private string GB2312_all = "阿呵啊嗬啊嗄阿啊阿啊吖阿呵啊腌锕嗬挨捱皑癌艾唉爱砹隘嗌嗳嫒碍暧瑷噫嗳矮蔼霭哎哀诶唉埃挨嗳锿犴岸按案胺暗黯俺埯铵揞厂广安桉氨庵谙鹌鞍仰昂盎肮敖嗷廒獒遨骜熬翱聱螯嚣鳌鏖岙坳拗傲奥骜澳懊鏊艹夭拗袄媪凹熬扒吧罢拔茇菝跋魃叭伯坝把杷爸罢耙靶鲅霸灞把钯靶八巴叭扒吧岜芭疤捌笆粑白呗败拜稗百伯佰陌柏捭摆掰擘办半伴扮拌绊瓣阪坂板版钣舨扳班般颁斑搬瘢癍旁蚌傍棒谤蒡榜膀磅镑绑榜膀邦帮梆浜彭雹薄刨报抱豹趵鲍暴瀑曝爆宝饱保鸨堡葆褓褒勹包孢苞炮胞剥煲龅褒呗臂贝北孛狈邶备勃背钡倍悖被惫焙辈碚蓓褙鞴鐾北陂卑杯背庳悲碑鹎夯体坌奔笨本苯畚奔贲锛甭泵迸蚌堋绷甏蹦绷崩绷嘣荸鼻币必毕闭佛庇芘拂泌畀哔毖荜贲陛毙狴秘铋婢庳敝萆弼愎筚滗痹睥蓖裨跸辟弊碧箅蔽壁嬖篦薜避濞臂髀璧襞匕比吡妣纰彼秕俾笔舭鄙逼卞弁忭汴苄便变缏遍辨辩辫贬扁窆匾碥褊边砭笾编煸蝙鳊鞭鳔表婊裱灬杓彪标飑髟骠膘瘭镖飙飚镳别蹩别瘪憋瘪鳖摈殡膑髌鬓份玢宾彬傧斌滨缤频槟镔濒豳并枋病摒丙邴秉屏柄炳饼绠禀冫冰并兵屏槟卜啵膊伯佛孛驳帛泊勃柏亳钹铂脖舶博渤鹁搏箔膊魄踣薄簿礴柏擗薄擘檗簸跛簸拨波玻剥般趵钵饽啵菠鲅播蕃醭不布步怖钚埔部埠瓿簿卜卟补哺捕堡逋钸晡礤拆嚓擦才材财裁采菜蔡采彩睬踩猜残蚕惭灿掺孱粲璨惨黪参骖餐臧藏仓伧沧苍舱曹嘈漕槽艚螬屮草操糙册侧厕恻栅测策岑涔参层曾蹭噌叉苴茌查茬茶搽猹楂槎察碴檫叉汊岔杈刹衩诧咤姹差叉衩镲叉杈差喳插馇碴锸嚓侪柴豺差虿瘥拆钗差单婵崭谗孱禅馋缠蝉廛潺澶蟾躔忏颤羼产谄铲阐蒇骣冁觇掺搀场长场肠苌尝倘偿常徜嫦裳怅畅倡鬯唱厂场昶惝淌敞氅伥昌倡娼猖菖阊鲳晁巢朝嘲潮耖吵炒吵抄怊钞绰焯超剿屮彻坼掣撤澈尺扯车砗伧尘臣忱沈沉辰陈宸晨谌衬称龀趁榇谶碜肜抻郴琛嗔丞成呈承枨诚城乘埕晟盛铖惩程裎塍酲澄橙秤称逞骋惩裎抢净柽秤称蛏铛噌撑樘瞠弛池驰迟坻茌持匙墀踟篪彳叱斥赤饬炽眙翅敕啻傺瘛尺侈齿耻豉褫吃哧蚩鸱眵笞嗤媸痴螭魑虫种重崇冲铳宠充冲忡茺涌舂憧艟仇俦帱惆绸畴愁稠筹酬踌雠臭丑瞅抽瘳刍处助除厨滁锄蜍雏橱躇蹰亍处怵绌畜搐触憷黜矗处杵础储楮楚褚出初樗啜揣嘬膪踹揣揣搋传舡船遄椽串钏舛喘巛川氚穿床幢创怆闯创疮窗垂陲捶棰椎槌锤吹炊纯肫唇莼淳鹑醇朐蠢春椿蝽绰辶啜淖绰辍龊踔戳词兹祠茈茨瓷粢慈辞磁雌鹚糍次伺刺赐此刺呲差疵从丛淙琮匆囱苁枞葱骢璁聪凑楱腠辏徂殂卒促猝趋酢蔟趣醋簇蹙蹴粗攒窜篡镩爨汆撺镩蹿脆啐悴淬萃毳瘁粹翠璀隹衰崔催摧榱存蹲寸忖村皴嵯痤矬瘥鹾厝挫措锉错脞搓磋撮蹉疸塔瘩打达妲怛沓惮笪答瘩靼鞑大打哒耷搭答嗒褡大代诒岱甙绐迨隶骀带待怠殆毒玳贷埭袋逮棣戴黛歹逮傣呆呔待旦石但担诞啖弹惮淡萏蛋氮瘅儋澹膻担胆疸掸赕丹单担眈耽郸聃殚瘅箪儋凼当宕砀挡荡档菪挡党谠当裆铛到倒帱悼焘盗道稻纛导岛倒捣祷蹈刀刂叨忉氘地底的得得锝德得邓凳嶝澄瞪磴镫蹬等戥灯登噔簦蹬狄的籴迪敌涤荻笛觌嘀嫡翟镝地弟的帝娣递第谛棣睇缔蒂碲氐诋邸坻底抵柢砥提骶氐低羝堤提嘀滴镝嗲点电佃甸阽坫店垫玷钿惦淀奠殿靛癜簟典点碘踮掂滇颠巅癫吊钓调掉铞铫鸟刁叼凋貂碉雕鲷佚迭垤瓞谍喋堞揲耋叠牒碟蝶蹀鲽爹跌订钉定啶铤腚碇锭顶酊鼎丁仃叮玎町疔盯钉耵酊丢铥动冻侗垌峒恫栋洞胨胴硐董懂东冬咚岽氡鸫斗豆读逗渎痘窦斗抖钭陡蚪都兜蔸篼毒独读顿渎椟牍犊黩髑芏妒杜肚度渡镀蠹肚竺笃堵赌睹都嘟督髑段断缎椴煅锻簖短端队对兑怼敦碓憝镦追堆镦囤沌炖盾砘钝顿遁盹趸吨敦墩礅镦蹲掇夺度铎踱驮剁沲垛柁堕舵隋惰跺隳朵哚垛缍躲多咄哆掇裰呃讹俄哦娥峨莪锇鹅蛾额厄呃扼苊轭垩恶胺饿谔鄂阏愕萼遏腭锷鹗颚噩鳄恶阿婀屙诶诶诶诶摁恩蒽儿儿而鸸鲕二佴贰铒尔耳佴迩洱饵珥铒乏伐垡罚阀筏发珐法砝发凡矾钒烦袢樊蕃燔繁蹯蘩犯泛饭范贩畈梵反返帆拚番幡蕃翻藩防坊妨房肪鲂放仿访彷纺舫匚方邡坊芳枋钫肥淝腓痱发吠芾废怫沸狒肺费砩痱镄匪诽悱菲斐榧翡蜚篚飞妃非啡绯菲扉斐蜚霏鲱坟汾棼焚鼢分份奋忿偾愤粪鲼瀵粉分吩纷芬氛玢酚冯逢缝凤风奉俸葑缝风讽唪丰风沣枫封疯砜峰烽葑锋蜂酆佛不缶否夫弗伏凫佛孚扶芙芾宓怫拂服绂绋苻俘氟祓罘茯郛浮砩莩蚨匐桴涪符艴菔袱幅跗福蜉辐幞蝠黻父讣付妇负附咐服阜驸复赴副傅富赋缚腹鲋赙蝮鳆覆馥阝父呒抚甫府拊斧俯釜脯辅腑滏腐黼夫呋孚肤趺麸稃跗孵敷轧钆尜嘎噶尬尕嘎夹旮伽呷咖嘎丐芥钙盖溉戤概改该陔垓赅干旰绀淦赣杆秆赶敢感澉橄擀干甘奸杆肝坩泔矸苷柑竿疳酐乾尴岗杠钢筻戆岗港冈刚扛岗杠纲肛缸钢罡告诰郜锆膏杲搞缟槁稿镐藁皋羔高槔睾膏篙糕阁革格胳鬲搁葛蛤隔颌嗝塥搿膈镉骼髂个各虼硌铬个各合哿盖舸葛戈仡圪纥疙咯哥格胳袼鸽割搁歌给哏亘艮茛艮根跟更哽埂绠耿梗颈鲠更庚耕赓羹共贡供赣廾共巩汞拱珙工弓公功共红攻供肱宫恭蚣躬龚觥勾构诟购垢够媾彀遘觏岣狗苟枸笱勾句佝沟拘枸钩缑篝鞲鹘估固故顾崮梏牿雇痼锢鲴古汩诂谷股苦牯骨罟贾钴蛄蛊鹄毂鼓嘏鹘臌瞽估呱咕姑孤沽轱骨家鸪菇菰蛄觚辜酤毂箍卦诖挂褂呙呱剐寡瓜刮呱括胍栝鸹怪拐乖掴观串贯冠惯掼涫盥灌鹳罐莞馆斡管关观纶官冠矜倌莞涫棺鳏桄逛广犷光咣桄胱潢刽刿柜炅炔贵桂桧匮眭跪鳜宄轨庋匦诡癸鬼晷簋归圭妫龟规皈闺傀硅瑰鲑棍丨衮绲棍辊滚磙鲧过囗国帼掴虢馘过果椁蜾裹过呙埚涡郭崞猓聒锅蝈虾蛤哈哈呵哈铪还咳孩骸亥骇害氦海胲醢咳嗨邗汗含邯函晗涵焓寒韩厂汉汗含旰旱悍捍焊菡感颔撖憾撼翰瀚罕喊犴顸蚶酣憨鼾行吭杭肮绗桁珩航颃行沆巷夯炕号皋蚝毫嗥貉豪嚎壕濠号好昊浩耗皓镐颢灏好郝蒿嚆薅禾合纥何呙劾和河曷阂害核盍荷涸盒盖菏颌貉阖翮吓何和贺荷喝鹄褐赫鹤壑何诃呵苛喝嗬黑嗨嘿痕恨哏很狠行恒桁珩横衡蘅横亨哼哼弘红宏闳泓洪荭虹鸿蕻黉讧哄蕻哄轰哄訇烘薨侯喉猴瘊篌糇骺后侯厚後逅候堠鲎吼囫和弧狐胡壶核斛湖猢葫鹄煳瑚鹕鹘槲糊蝴醐觳互户冱护沪岵怙戽虎祜笏扈瓠雇糊鹱许虎浒唬琥乎戏虍芴呼忽烀轷唿惚滹糊划华哗骅铧滑猾鲑化划华画话桦化华花哗砉划怀徊淮槐踝坏还环郇洹垸桓萑锾圜寰缳鬟幻奂宦唤换浣涣眩患焕逭痪豢漶鲩擐缓欢獾荒慌皇凰隍黄徨惶湟遑煌潢璜篁蝗癀磺簧蟥鳇晃恍荒晃谎慌幌肓荒慌回洄茴缋蛔卉汇会讳诙哕浍绘荟诲恚桧烩贿彗晦秽喙惠溃缋慧蕙蟪虫虺悔毁戏灰炜诙咴恢挥虺晖珲堕辉睢麾徽隳浑珲混馄魂诨浑混棍溷昏荤婚阍曛伙和夥和活豁和或货获祸惑锪霍豁镬嚯藿蠖火灬伙钬夥耠锪劐豁攉辑箕及吉岌汲级即极亟佶诘急笈革疾嵇戢棘殛集嫉嵴楫蒺辑瘠蕺藉籍计记伎纪齐妓忌技系芰际剂季哜既洎济荠迹继觊偈寂寄悸祭绩蓟暨跽霁鲚稷鲫冀髻骥几己纪虮挤济给脊掎戟嵴麂几丌讥击叽饥乩圾机玑肌芨矶鸡其奇咭剞唧姬屐积笄基嵇期犄缉赍嫉畸跻箕畿稽齑墼激羁家甲夹郏拮荚恝戛袷铗蛱颉颊价驾架贾假嫁稼甲岬胛贾钾假嘏瘕加夹伽佳茄迦挟枷浃珈哿家痂笳袈葭跏嘉镓见件间建饯剑牮荐贱健涧监舰堑渐谏楗毽溅腱践锏鉴键僭槛箭踺囝拣枧俭柬茧捡笕减剪检趼睑硷裥锏简谫戬碱翦謇蹇戋奸尖坚歼间浅肩艰兼监渐笺菅湔溅犍缄搛煎缣蒹鲣鹣鞯匠降将洚绛浆强酱犟糨讲奖桨港蒋耩江姜将茳浆豇僵缰礓疆矫嚼叫峤觉校轿较教窖酵噍徼醮嚼角佼侥挢狡绞饺皎矫脚铰搅湫剿敫徼缴艽交郊姣娇浇茭骄胶教椒焦蛟跤僬鲛蕉徼礁鹪价家卩孑节讦劫杰诘拮洁结桀桔偈婕捷接袷颉睫截碣竭鲒羯介价戒芥届界疥诫借蚧解骱藉她姐解节阶疖哜皆结接秸喈嗟揭街楷斤仅尽劲妗近进荩晋浸烬赆缙禁靳觐噤仅尽卺紧堇谨锦廑馑槿瑾巾今斤钅金津矜衿堇筋禁襟劲净弪径经迳胫痉竞婧竟敬靓靖境獍静镜井阱刭肼颈景儆憬警京径泾经茎荆惊旌菁晶腈睛粳兢箐精鲸炅迥炯窘冂扃蹴旧臼咎疚柩桕厩救就舅僦鹫九久灸玖韭酒纠究鸠赳阄啾揪湫鬏矩榘局桔菊锔橘鞫句巨讵拒苣具沮炬钜俱倨剧惧据渠距犋飓锯窭聚屦踞遽瞿醵咀柜沮举枸矩莒匮榉榘龃踽车且居拘狙苴驹疽据掬蛆椐琚趄锔裾雎鞠鞫卷倦桊狷绢隽圈眷鄄卷锩娟捐涓圈鹃镌蠲孓决诀抉角珏绝觉倔崛掘桷脚觖厥阙劂谲撅獗蕨噱橛爵镢蹶嚼矍鳜爝攫倔噘撅俊郡峻捃浚隽骏菌竣军君均龟钧皲菌筠麇卡佧咔咯胩咔咖喀忾岂凯剀垲恺铠慨蒈楷锴开揩锎看嵌阚瞰凵坎侃砍莰槛刊看勘龛堪戡扛亢伉抗闶炕钪闶康慷糠铐犒靠考拷栲烤尻壳咳颏可克刻客恪课氪骒缂嗑溘锞可坷岢渴呵坷苛柯珂科轲疴钶蚵铪棵颏嗑嗬稞窠颗瞌磕蝌髁裉肯垦恳啃龈吭坑铿空控孔空倥恐空倥崆箜叩扣佝寇筘蔻口抠芤眍库绔喾裤酷苦刳挎枯哭堀窟骷夸挎胯跨侉垮夸会块快侩郐哙浍狯脍蒉筷蒯款宽髋狂诳邝圹纩况旷矿贶框眶夼匡诓哐筐奎逵隗馗喹揆葵暌魁睽蝰夔归匮喟愦愧溃蒉馈篑聩傀跬亏岿悝盔窥困悃捆阃坤昆琨锟髡醌鲲扩括栝蛞阔廓啦喇旯拉剌砬啦喇剌喇腊落瘌蜡辣癞拉喇垃拉砬啦喇邋来崃徕涞莱铼来疠徕赉睐赖濑癞籁兰岚拦栏啉婪阑蓝谰澜褴斓篮镧烂滥览揽缆榄漤罱懒郎狼莨廊琅榔稂锒螂郎浪莨阆蒗朗啷劳牢唠崂涝痨铹潦醪络唠涝烙落耢酪潦老佬姥栳铑潦獠捞了仂乐叻泐勒鳓肋嘞勒嘞累雷嫘缧擂儡檑镭羸肋泪类累雷酹擂檑耒诔垒累漯磊蕾儡勒塄棱楞愣楞冷棱哩丽厘梨狸离骊犁喱鹂漓缡蓠蜊嫠璃鲡黎篱罹藜黧蠡力历厉立吏丽利励呖坜沥苈例戾枥疠隶俐俪栎疬荔轹郦栗猁砬砺砾莅莉鬲唳笠粒粝蛎傈痢詈跞雳溧蜊篥礼李里俚哩娌悝逦理锂鲤澧醴蠡鳢哩俩奁连帘怜涟莲联裢廉鲢濂臁镰蠊练炼恋殓链楝潋莶敛琏脸裣蔹量良凉莨梁椋量粮粱墚踉亮凉谅辆晾量靓踉两俩魉辽疗聊僚寥嘹寮撩潦獠缭燎鹩尥钌料廖撂撩燎镣了钌缪蓼撩燎撩咧咧列劣冽洌埒烈捩猎裂趔躐鬣咧裂咧邻林临啉淋琳粼嶙遴辚霖瞵磷鳞麟吝临赁啉淋蔺膦躏凛廪懔檩拎令伶灵呤囹岭泠苓柃玲瓴凌铃陵棂绫羚翎聆菱蛉棱零龄鲮酃令另呤令岭领刘浏流留琉硫旒遛馏骝榴熘瘤镏鎏六陆溜碌遛馏镏鹨柳绺锍溜熘咯龙咙泷茏栊珑胧砻笼聋隆癃窿弄陇垄垅拢笼喽娄偻喽蒌楼耧蝼髅陋漏瘘镂露嵝搂篓搂轳氇卢庐芦垆泸炉栌胪鸬舻颅鲈六陆录赂辂渌绿逯鹿禄碌路漉蓼戮辘潞璐簏鹭麓露卤虏掳鲁橹氇镥噜撸驴闾榈律虑率绿氯滤吕侣捋旅偻稆铝屡缕膂褛履娈孪峦挛栾鸾脔滦銮乱卵掠略锊仑伦论囵抡沦纶轮论抡罗猡脶萝逻椤锣箩骡镙螺蠃蠡泺咯洛络荦骆烙珞硌落跞摞漯雒倮裸瘰蠃捋落么吗嘛麽蟆毛吗麻蟆杩蚂骂唛马吗犸杩玛码蚂妈抹蚂麻摩埋霾劢迈麦卖脉买荬埋蛮谩蔓馒瞒鞔鳗曼谩墁幔慢漫缦蔓熳镘满螨埋颟邙忙芒氓盲茫硭莽漭蟒邙毛矛牦茅茆旄猫锚髦蝥蟊茂冒贸旄耄袤帽瑁瞀貌懋卯峁泖茆昴铆猫么麽没枚玫眉莓梅媒嵋湄猸楣煤酶镅鹛霉糜妹昧袂谜媚寐魅每美浼镁们门们扪汶钔瞒闷焖鞔懑闷氓虻萌盟蒙甍瞢朦檬礞艨孟梦黾勐猛蒙锰艋蜢瞢蟒懵蠓蒙弥祢迷猕眯谜醚糜縻麋靡蘼冖糸汨宓泌觅秘密幂谧嘧蜜米芈弥咪弭敉脒眯靡咪眯宀眠绵棉瞑眄面免沔黾勉眄娩冕渑湎缅腼苗描瞄鹋妙庙缪杪眇秒淼渺缈藐邈喵灭蔑篾蠛乜芈咩民岷玟苠珉缗皿闵抿泯黾闽悯敏愍鳘名明鸣茗冥铭溟暝瞑螟命酩谬缪无谟嫫馍摹模膜麽摩磨嬷蘑魔万末没抹殁沫茉陌冒脉秣莫袜寞漠蓦貉貊嘿墨瘼镆磨默貘耱万抹摸牟侔眸谋蛑缪鍪厶某哞毪模木仫目牟沐坶牧苜莫钼募墓幕睦慕缪暮穆母亩牡坶姆拇姥哪拿镎内那呐纳肭娜衲钠捺那哪那南佴奈柰耐能萘鼐乃奶艿氖哪男南难喃楠难赧腩蝻囝囡囔囊馕曩攮馕囊囔呶挠桡硇铙猱蛲闹淖垴恼脑瑙孬呢哪疒讷那呐内那哪馁恁嫩而能唔嗯唔嗯唔尼呢坭怩泥倪铌猊霓鲵伲泥昵逆匿溺睨腻嶷尼伲你拟旎疑妮年粘鲇鲶黏廿念埝捻辇辗撵碾拈蔫娘酿尿脲溺鸟茑袅嬲乜陧涅聂臬啮嗫摄镊镍颞蹑孽蘖捏捻恁您宁咛拧狞柠聍甯凝宁佞拧泞甯拧牛拗忸扭狃纽钮妞农侬哝浓脓弄耨奴孥帑驽怒努呶弩胬恧衄女钕暖疟虐那娜挪难傩诺喏搦锘懦糯哦哦喔噢呕怄沤呕禺偶耦藕区讴呕沤欧殴瓯鸥扒杷爬钯耙琶筢帕怕叭派趴啪葩俳徘排牌哌派湃蒎迫排拍丬爿胖般盘磐蹒蟠伴判拌拚泮叛盼胖畔袢襻扳拌番潘攀仿彷庞逄旁膀磅螃胖耪乓滂膀刨咆庖狍炮袍匏跑泡炮疱跑抛泡脬坏陪培赔锫裴妃沛佩帔旆配淠辔霈坏呸胚醅喷盆湓喷喷朋堋彭棚硼蓬鹏澎篷膨蟛碰捧亨怦抨苹砰烹嘭皮仳纰芘陂枇毗疲罢蚍郫铍陴啤埤琵脾裨罴蜱貔鼙屁副埤淠媲睥辟僻濞甓貔譬匹庀疋仳圮吡否痞劈擗癖丕坏批纰邳坯披砒被铍陴劈噼霹便骈胼缏褊蹁片骗谝片扁偏犏篇翩朴嫖瓢票嘌漂骠莩殍漂缥瞟票剽嘌嫖漂缥飘螵丿苤撇氕撇瞥贫嫔频颦牝娉聘品榀拚姘拼冯平评凭坪苹屏枰瓶萍鲆乒俜娉婆鄱皤繁朴迫珀破粕魄叵钷笸朴钋陂坡泊泺泼颇掊裒掊剖仆匍莆脯菩葡蒲璞濮镤堡铺暴瀑曝朴圃埔浦普溥谱氆镨蹼仆攴攵扑铺噗啐丌亓伎祁齐圻岐芪其奇枝歧祈俟荠耆脐颀崎淇畦萁骐骑棋琦琪祺蛴旗綦蜞蕲鳍麒气讫汔迄弃汽亟妻泣契砌揭葺碛槭器憩乞企屺岂芑启杞起绮綮稽七沏妻柒凄栖桤戚萋期欹欺缉嘁漆蹊咭恰洽髂卡掐袷葜欠前荨钤虔钱钳乾掮犍箝潜黔欠纤芡茜倩堑嵌椠慊歉凵浅肷遣谴缱千仟阡扦芊迁佥岍钎牵悭铅谦愆签骞搴褰强墙嫱蔷樯疆呛戗炝跄抢羟强襁镪呛抢羌戕戗枪将跄腔蜣锖锵镪乔侨峤荞桥翘劁谯憔蕉鞒樵瞧壳俏诮峭窍翘谯撬鞘巧悄雀愀悄硗雀跷劁敲锹橇缲趄伽茄切妾怯郄契砌窃挈惬趄慊箧锲且切芩芹矜秦琴禽覃勤嗪溱廑噙擒檎螓吣沁揿锓寝亲侵钦衾情晴氰擎檠黥庆亲倩箐綮磬罄苘顷请謦青顷氢轻倾卿圊清蜻鲭邛穷穹茕筇琼蛩跫銎銎仇囚犰艽求虬泅俅酋逑球赇巯遒裘蝤鼽糗丘邱龟秋蚯湫楸鳅戌劬朐鸲渠蕖磲璩瞿蘧氍癯衢蠼去阒觑趣曲苣取娶龋麴区曲岖诎驱屈祛蛆躯蛐趋觑麴黢全权卷诠泉荃拳辁痊铨筌蜷醛鬈颧劝券犭犬畎绻悛圈瘸芍却悫雀确阕阙鹊榷炔缺阙裙群麇逡然蚺然髯燃冉苒染禳瓤穰让嚷壤攘穰嚷娆荛饶桡绕扰娆热若喏惹人亻仁壬任刃认仞任纫妊轫韧饪衽葚忍荏稔仍艿扔日戎肜狨绒茸荣容嵘溶蓉榕熔蝾融冗柔揉糅蹂鞣肉如茹铷儒嚅孺濡薷襦蠕颥入洳溽缛蓐褥女汝乳辱阮朊软蕤兑芮枘蚋锐瑞睿蕊闰润若偌弱箬挲卅飒脎萨洒撒仨撒塞赛思塞腮噻鳃散散伞散糁馓三叁参毵丧丧嗓搡磉颡丧桑扫梢埽瘙燥臊扫埽嫂骚搔骚缫缲臊鳋色涩啬铯塞瑟槭穑森僧挲啥奢沙唼厦嗄歃煞霎傻杀杉沙纱刹砂莎挲铩痧煞裟噎鲨晒色筛酾讪汕单姗疝苫钐剡扇掸善禅骟鄯缮嬗擅膳赡蟮鳝闪陕掺山彡删杉芟姗苫衫钐埏栅珊舢扇掺跚煽潸膻裳上尚绱上垧晌赏伤汤殇商觞墒熵勺芍杓苕韶少召劭邵绍哨捎稍潲少捎梢烧稍筲艄蛸鞘舌佘折蛇揲叶厍设社舍拾射涉赦慑摄滠歙麝舍奢猞赊畲谁什甚神肾甚胂渗葚慎椹蜃吲沈审哂矧谂婶渖申伸身参呻绅诜信哂娠砷莘深糁渑绳圣甸胜乘晟盛剩嵊省眚升生声牲笙甥栅匙殖十饣什石时识实拾炻蚀食埘莳硕鲥士氏礻世仕市示式似事侍势泽视试饰室峙恃拭是柿贳适耆舐莳轼逝铈弑谥释嗜筮誓噬螫史矢豕使始驶屎尸失师虱诗施狮湿蓍嘘酾鲺熟寿受狩兽售授绶瘦手扌守首艏收术朱秫孰赎塾熟术戍束沭述俞树竖恕庶数腧墅漱澍属暑黍数署鼠蜀薯曙书殳疋抒纾叔枢姝倏殊荼梳淑菽疏舒摅毹输蔬刷耍刷唰帅率蟀甩衰摔涮闩拴栓爽双泷爽霜孀谁说税睡氵水顺舜瞬吮妁烁朔铄硕搠数蒴槊说厕巳四寺汜伺似兕姒祀泗饲驷俟食笥耜嗣肆死厶纟丝司糸私咝思鸶斯缌蛳厮锶嘶撕澌讼宋诵送颂怂悚耸竦忪松凇崧淞菘嵩嗽擞叟嗾瞍螋擞薮叟嗖搜溲馊飕锼艘螋俗夙苏诉肃涑素速宿粟谡嗉塑愫溯僳缩蔌觫簌苏酥稣蒜算狻酸荽绥隋随遂岁祟谇遂碎粹隧燧穗邃髓尿虽荽眭隋睢濉损笋隼榫孙狲荪飧嗦些所唢索琐锁唆娑莎挲桫梭睃嗍羧蓑缩沓遢拓沓挞闼嗒遢榻漯踏蹋塔獭鳎他它她趿铊塌溻踏台邰抬苔骀炱跆鲐薹大太汰态肽钛泰酞呔台苔胎坛昙谈郯弹覃痰锬谭潭澹檀叹炭探碳忐坦袒钽毯坍贪摊滩瘫唐堂棠塘搪溏瑭樘膛糖螗镗螳醣汤烫铴趟帑倘惝淌傥耥躺汤铴羰趟镗洮逃桃陶啕淘萄跳鼗套讨叨涛绦掏焘滔韬饕忑忒特铽慝疼腾誊滕藤荑绨啼提缇鹈题蹄醍弟屉剃倜悌涕绨逖惕替棣裼嚏体醍体剔梯锑踢田佃甸恬畋钿甜嗔填阗掭忝殄栝腆舔天添条苕迢调笤铫龆蜩髫鲦啁眺粜跳挑窕条佻挑祧帖餮帖铁帖贴萜廷亭庭莛停婷葶蜓霆梃町挺梃铤艇厅汀听町烃仝同佟彤侗垌峒茼桐砼铜童酮僮潼瞳同恸通痛侗统捅桶筒恫通嗵头亠头投骰透偷愉图徒涂荼途屠菟酴吐兔堍菟土吐钍凸秃突团抟揣彖疃湍弟颓退煺蜕褪腿忒推饨屯囤饨豚臀褪氽吞暾驮佗陀坨沱沲驼柁砣铊鸵跎酡橐鼍拓柝唾箨魄妥庹椭乇托佗拖说脱哇娃瓦袜腽瓦佤凹呙哇挖洼娲蛙外歪崴呙歪丸纨芄完玩顽烷万腕蔓宛挽晚莞婉惋绾脘菀琬皖畹碗箢弯剜湾蜿豌亡王王妄忘旺望网往枉罔惘辋魍尢汪囗为韦圩围帏沩违闱隹桅涠唯帷惟维嵬潍卫为未位味畏胃軎尉谓喂渭猬遗蔚慰魏伟伪尾纬苇委炜玮洧娓诿萎隗猥痿艉韪鲔危委威倭偎逶隈崴葳微煨薇巍文纹玟闻蚊阌雯问免汶纹璺刎吻紊稳温韫瘟瓮蕹蓊翁嗡沃肟卧媪幄握渥硪斡龌呙我挝倭涡莴喔窝蜗五亡无毋吴吾芜唔梧浯蜈鼯兀乌勿务戊阢坞杌芴物误恶悟晤焐婺痦骛雾寤鹜鋈五午仵伍妩庑忤怃迕武侮捂牾鹉舞兀乌圬污邬呜巫於屋诬钨恶习席袭觋媳褶隰檄戏忾系饩细郄阋舄隙禊洒洗玺徙铣喜葸屣蓰禧鳃夕兮汐西吸希昔析矽穸咭茜诶郗唏奚息栖浠牺悉惜欷淅烯硒菥晰犀稀粞翕腊舾溪皙裼锡僖熄熙蜥嘻嬉膝樨歙熹羲螅蟋蹊醯曦鼷夹匣侠狎峡柙狭假硖葭遐暇瑕瘕辖霞黠下吓夏唬厦罅呷虾瘕瞎闲弦贤咸涎娴舷衔痫鹇嫌见县岘苋现线限宪陷馅羡献腺寰霰彡冼显洗险猃蚬铣筅跣鲜藓燹仙先纤氙祆籼莶掀掺跹酰锨鲜暹羊详降庠祥翔向巷相项象像橡蟓享响饷飨想鲞乡芗相香厢湘缃葙箱襄骧镶姣崤淆孝肖俏哮效校笑啸小晓筱肖枭削哓枵骁宵消绡逍萧硝销蛸潇箫霄魈嚣叶协邪胁挟偕斜谐颉携鲑勰撷缬鞋写泄泻绁卸契屑械亵渫谢解榍榭廨懈獬薤邂燮瀣蟹躞写血些楔歇蝎镡囟芯信衅忄心忻芯辛昕欣莘锌新歆薪馨鑫刑行邢饧形陉型荥硎兴杏姓幸性荇悻省醒擤兴星惺猩腥雄熊凶兄匈芎汹胸秀岫绣臭袖宿锈嗅溴朽宿休修咻庥羞鸺貅馐髹蓿邪徐休旭序叙恤朐洫畜顼勖绪续酗婿溆絮煦蓄许诩浒栩糈醑吁圩戌盱砉胥须顼虚嘘需墟玄县痃悬旋漩璇券泫炫绚眩铉旋渲楦碹镟选癣轩宣谖喧揎萱暄煊儇穴学泶踅噱血谑彐雪鳕削靴薛寻巡旬询郇峋恂洵浔荀荨循鲟潭训讯孙汛迅驯徇逊殉浚巽熏蕈勋荤埙熏窨獯薰曛醺呀牙伢邪岈芽琊蚜崖涯睚衙轧亚压讶迓垭娅砑氩琊揠碣疋哑痖雅丫压厌吖呀押哑鸦桠鸭雅讠延闫严妍芫言阽岩沿炎埏研盐铅阎筵蜒颜檐厌咽彦研砚唁宴晏艳验谚堰焰焱雁滟酽谳餍燕赝兖奄俨衍剡偃厣掩眼郾琰罨演魇鼹奄咽恹殷烟胭崦淹焉菸阉阏湮腌鄢嫣燕扬羊阳杨炀佯疡详徉洋烊蛘炀怏恙样烊漾鞅仰养氧痒央泱殃秧鸯鞅爻尧侥肴姚荛轺珧皋陶窑窕铫谣徭摇遥瑶繇鳐疟药要钥鹞曜耀杳咬窈舀崾么幺夭吆约妖要腰徼邀爷邪耶揶铘业叶曳页邺夜咽拽射晔烨掖液谒喝揲腋靥也冶野耶掖椰噎翳义仪圯夷沂诒宜怡迤饴咦姨荑贻眙胰痍移蛇遗颐疑嶷彝义亿弋刈忆艺仡艾议亦屹异衣佚呓役抑译邑佾峄怿易泄疙绎诣驿奕弈疫羿轶食射悒挹益谊埸翊翌逸嗌意溢缢肄裔瘗蜴毅熠镒劓噫殪薏翳翼臆癔镱懿乙已以钇尾矣苡依迤舣蚁倚酏猗椅蛾旖一衤伊衣医依咿猗铱壹揖椅漪噫黟吟圻垠狺寅淫银鄞夤龈霪廴印饮茚胤荫隐窨廴尹引吲饮殷蚓隐瘾因阴姻洇茵荫音殷氤铟喑堙湮迎茔盈荥荧莹萤营萦楹滢蓥潆蝇嬴赢瀛应映硬媵郢景颍颖影瘿应英莺婴瑛嘤撄缨罂樱璎鹦膺鹰哩哟育哟唷喁用佣永甬咏泳俑勇涌恿蛹踊佣拥痈邕庸雍墉慵壅镛臃鳙饔尢尤由犹邮油柚疣莜莸铀蚰游鱿猷蝣蝤繇又右幼有佑侑囿宥柚诱蚴裒釉鼬友有卣酉莠铕牖黝优忧攸呦幽悠燠于与予邪余妤欤於盂臾鱼俞禺竽舁娱狳谀馀渔萸隅雩喁嵛愉揄渝腴逾愚榆瑜虞觎窬舆蝓与肀玉驭吁聿芋吾妪汩谷饫育郁雨俞昱狱禺语峪浴钰预域尉欲菀谕阈喻奥寓御粥裕遇鹆愈煜蓣誉毓蔚蜮熨豫燠鹬鬻与予伛宇屿羽妪雨俣禹语圄圉庾瘐窳龉吁纡迂於淤菸瘀元员园沅芫垣爰原圆袁媛援缘鼋塬源猿辕圜橼螈远苑怨院垸媛掾缘瑗愿远宛鸢冤眢鸳渊箢悦月乐刖兑岳栎说钥悦钺阅跃粤越樾龠瀹哕曰约云匀员纭芸昀郧耘筠孕员芸运郓恽晕菀酝媪愠韫韵熨蕴允狁陨殒晕氲杂咱砸咋扎匝咂拶再在载仔宰载崽灾甾哉栽咱咱堑暂赞錾瓒拶昝攒趱糌簪奘脏葬臧藏驵脏赃臧凿灶皂唣造噪燥躁早枣蚤澡藻遭糟则咋择泽责迮啧帻笮舴箦赜仄侧昃贼谮怎综锃缯甑赠曾增憎缯罾扎札轧闸炸铡喋乍吒诈咋怍咤柞栅炸痄蚱槎榨蜡眨砟扎吒咋查哳喳揸渣猹楂齄瘵宅择翟责债柴砦祭寨瘵窄侧哜斋摘蘸占战栈站绽湛颤蘸斩展盏崭搌辗占沾毡旃粘詹谵瞻丈仗帐张杖胀账涨障嶂幛瘴仉长涨掌仉张章鄣嫜彰漳獐樟璋蟑着召兆诏赵笊棹照罩肇濯爪找沼钊招昭啁着朝嘲着乇折哲辄蛰蜇谪摺磔辙这柘浙蔗鹧者褚锗赭褶折蜇遮螫这圳阵鸩振朕赈填镇震诊枕轸畛疹缜稹贞针侦帧浈珍胗桢真砧祯斟椹溱甄蓁榛箴臻正争证怔诤郑挣政症钲铮拯整丁正争征怔诤峥挣政狰症钲睁铮筝蒸徵鲭执侄拓直郦值埴职植殖絷跖摭踯至志忮识豸制帙帜治炙知织质郅峙栉陟挚桎秩致贽轾掷痔窒鸷彘智滞痣蛭骘稚置雉膣觯踬夂止只旨址纸芷祉咫指枳轵趾黹酯徵之支氏卮只汁芝吱枝知织肢枳栀祗胝脂蜘中仲众种重衷肿种冢踵夂中忪忠终盅钟舯蚣衷锺螽碡妯轴纣咒宙绉昼祝胄荮轴皱酎舳繇骤籀肘帚州舟诌周洲调啁粥主舳术竹竺柚烛逐舳筑瘃躅主宁伫住助苎杼注贮驻柱炷祝疰庶著蛀甯筑铸箸翥澍丶主拄柱渚属煮褚嘱麈瞩朱侏诛邾洙茱株珠诸猪铢蛛槠潴橥爪抓挝拽转拽传沌转啭巽赚撰篆馔转专砖颛壮状僮幢撞戆奘妆庄桩装坠缀惴缒赘隹追骓揣椎锥准屯肫窀谆淳灼卓茁斫浊浞诼酌啄着著琢禚箸缴擢濯镯躅拙倬捉桌涿淖棹焯擢子字自恣渍眦子仔姊秭籽耔茈笫梓紫滓訾仔吱孜甾兹呲咨姿恣赀资淄缁谘孳嵫滋粢辎觜訾趑锱龇髭鲻从纵粽纵总偬宗枞综棕腙踪鬃奏揍楱走邹驺诹陬鄹鲰足卒族镞诅阻组俎祖苴租菹钻赚攥缵纂钻躜最罪蕞醉咀觜嘴咀羧撙尊遵樽鳟昨笮琢作坐阼怍柞祚胙唑座做酢左佐撮作嘬呒姆唔嗯";


        const int MAX_BASIC_STROKE = 32;
        //Total 32 笔画， 生成64x64的Normalized的Region，对笔画命名
        enum StrokeName
        {
            HENG, SHU, PIE, DIAN, HENGZHE, NA, TI, HENGZHEGOU, SHUGOU, HENGPIE,
            HENGGOU, SHUWANGOU, PIEZHE, SHUTI, SHUZHE, PIEDIAN, SHUZHEZHEGOU, XIEGOU, HENGPIEWANGOU, HENGZHETI,
            WANGOU, HENGZHEWANGOU, SHUWAN, HENGZHEWAN, HENGZHEZHEGOU, HENGXIEGOU, HENGZHEZHEPIE, SHUZHEPIE, SHUZHEZHE, HENGZHEZHE,
            HENGZHEZHEZHE, WOGOU
        }

        string[] StrokeNameInChinese = {"横","竖","撇","点","横折","捺","提","横折钩","竖钩","横撇",
                                  "横钩","竖弯钩","撇折","竖提","竖折","撇点","竖折折钩","斜钩","横撇弯钩","横折提",
                                  "弯钩","横折弯钩","竖弯","横折弯","横折折折钩","横斜钩","横折折撇","竖折撇","竖折折","横折折",
                                  "横折折折","卧钩", "没发现"};
        string[] StrokeInChinese = {"大", "十", "八", "主", "口", "人", "地", "月", "小", "水",
                                "你", "元", "去", "良", "山", "女", "弟", "我", "那", "课",
                                "了", "九", "四", "没", "仍", "凰", "及", "专", "鼎", "凹", "凸","心"};

        //有些笔画在字库中有两个SubPath
        //笔画值不想等，则需要两个笔画
        int[] StrokeIndexInKaitiGB2312_a = {    0, 0, 0, 0, 2, 1, 5, 1, 1, 0,
                                3, 3, 4, 5, 2, 2, 3, 3, 5, 8,
                                1, 0, 2, 0, 3, 1, 0, 0, 11, 2, 1,3 };
        int[] StrokeIndexInKaitiGB2312_b = {    0, 0, 0, 0, 2, 1, 5, 1, 1, 0,
                                3, 3, 4, 5, 2, 2, 3, 3, 5, 8,
                                1, 0, 2, 0, 3, 1, 0, 0, 12, 3, 1,3 };

        //记录笔画的基本特征
        //包围盒特征
        //总的像素
        //纵横比
        //n x n网格编码
        List m_32BasicStrokes = new List();
        List m_32BasicStrokenBounds = new List();
        List m_32BasicStroken_Area = new List();

        public void PreCalc32Strokes()
        {
            if (m_32BasicStrokenBounds.Count > 0)
            {
                return;
            }

            //
            FontFamily fontFamily = new FontFamily(@"楷体_GB2312");
            int fontSize = 512;
            bool iscolse = false;

            for (int i = 0; i < MAX_BASIC_STROKE; i++)
            {
                GraphicsPath charPath = new GraphicsPath();
                charPath.AddString(StrokeInChinese[i], fontFamily, 0, fontSize, new PointF(0, 0), null);
                GraphicsPathIterator iterator;
                iterator = new GraphicsPathIterator(charPath);
                int count = iterator.SubpathCount;
                iterator.Rewind();

                GraphicsPath bihua = new GraphicsPath();
                for (int j = 0; j < count; j++)
                {
                    GraphicsPath subpath = new GraphicsPath();
                    iterator.NextSubpath(subpath, out iscolse);

                    if (StrokeIndexInKaitiGB2312_a[i] == j || StrokeIndexInKaitiGB2312_b[i] == j)
                    {
                        bihua.AddPath(subpath, false);
                    }
                    if (j == StrokeIndexInKaitiGB2312_b[i])
                        break;
                }
                //Compute Bounds of Path
                RectangleF b;
                b = bihua.GetBounds();

                //Transform to 64x64
                Matrix translateMatrix = new Matrix();
                float scale = 64.0f / Math.Max(b.Width, b.Height);
                //Translate by Top Left
                translateMatrix.Translate(-b.X * scale, -b.Y * scale);
                //Scale by MAX (W/h)               
                translateMatrix.Scale(scale, scale);
                bihua.Transform(translateMatrix);

                b = bihua.GetBounds();

                //Create Region               
                Region r = new Region(bihua);
                //translateMatrix.Reset();               
                //RegionData myRegionData = r.GetRegionData();

                int area = 0;
                for (int x = 0; x < 64; x++)
                    for (int y = 0; y < 64; y++)
                    {
                        if (r.IsVisible(x, y))
                        {
                            area++;
                        }
                    }
                m_32BasicStrokes.Add(bihua);
                m_32BasicStrokenBounds.Add(b);
                m_32BasicStroken_Area.Add(area);

                iterator.Dispose();
                charPath.Dispose();
            }
        }




        //Test by 提, 点, 草,新,
        public bool IsTi(ref GraphicsPath orig)
        {
            bool isTi = false;

            //transform to 64x64
            GraphicsPath path = (GraphicsPath)orig.Clone();
            RectangleF b = path.GetBounds();

            //Normalize
            //Transform to 64x64
            Matrix translateMatrix = new Matrix();
            float scale = 64.0f / Math.Max(b.Width, b.Height);

            //Translate by Top Left
            translateMatrix.Translate(-b.X * scale, -b.Y * scale);

            //Scale by MAX (W/h)               
            translateMatrix.Scale(scale, scale);
            path.Transform(translateMatrix);
            b = path.GetBounds();

            if (b.Width < 24 || b.Height < 24)
            {
                isTi = false;
                return isTi;
            }


            Region r = new Region(path);
            //和上下两个矩形相交
            GraphicsPath mask = new GraphicsPath();
            RectangleF rect = new RectangleF();
            rect.X = 0;
            rect.Y = 0;
            rect.Width = 64;
            rect.Height = 8;

            mask.AddRectangle(rect);
            rect.X = 0;
            rect.Y = b.Height - 8;
            rect.Width = 64;
            rect.Height = 8;
            mask.AddRectangle(rect);

            r.Intersect(mask);
            Matrix m = new Matrix();
            RectangleF[] rects = r.GetRegionScans(m);

            float top_x = 64, top_w = 0;
            float bottom_x = 64, bottom_w = 0;

            for (int i = 0; i < rects.Count(); i++)
            {
                if (rects[i].Y < 8)
                {
                    top_x = Math.Min(top_x, rects[i].X);
                    top_w = Math.Max(top_w, rects[i].Width);
                }
                else
                {
                    bottom_x = Math.Min(bottom_x, rects[i].X);
                    bottom_w = Math.Max(bottom_w, rects[i].Width);
                }

            }
            //
            //撇点捺
            // Top-right
            //Bottom_left
            //Not 点
            //not 捺
            isTi = (top_x > 45.0 / 64 * b.Width) && bottom_x < 20.0 / 64 * b.Width && top_w < 15.0 / 64 * b.Width && bottom_w > 15.0 / 64 * b.Width;


            return isTi;
        }

        public void GeneratePath(ref List pathlist, string str)
        {
            FontFamily fontFamily = new FontFamily(@"楷体_GB2312");
            int fontSize = 512;
            GraphicsPath charPath = new GraphicsPath();
            charPath.AddString(str, fontFamily, 0, fontSize, new PointF(0, 0), null);
            GraphicsPathIterator iterator;
            iterator = new GraphicsPathIterator(charPath);
            int count = iterator.SubpathCount;
            iterator.Rewind();

            bool iscolse;
            for (int i = 0; i < count; i++)
            {
                GraphicsPath subpath = new GraphicsPath();
                iterator.NextSubpath(subpath, out iscolse);
                pathlist.Add(subpath);
            }
        }


        public void GenAnimatepath_Test()
        {
            PreCalc32Strokes();


            for (int i = 0; i < m_32BasicStrokes.Count; i++)
            {
                GraphicsPath o = (GraphicsPath)m_32BasicStrokes[i].Clone();
                List animates = new List();
                GenAnimatepath(ref o, ref animates);
            }
        }


        public bool IsLeftToRight(ref GraphicsPath orig)
        {

            GraphicsPath path = (GraphicsPath)orig.Clone();
            RectangleF b = path.GetBounds();

            //Normalize
            //Transform to 64x64
            Matrix translateMatrix = new Matrix();
            float scale = 64.0f / Math.Max(b.Width, b.Height);

            //Translate by Top Left
            translateMatrix.Translate(-b.X * scale, -b.Y * scale);

            //Scale by MAX (W/h)               
            translateMatrix.Scale(scale, scale);
            path.Transform(translateMatrix);

            Region r = new Region(path);
            Matrix m = new Matrix();
            RectangleF[] rects = r.GetRegionScans(m);

            //Trace Scan from Top/Left to Bottom.
            float top_x = 64, top_w = 0;
            for (int i = 0; i < rects.Count(); i++)
            {
                if (rects[i].Y < 15)
                {
                    top_x = Math.Min(top_x, rects[i].X);
                    top_w = Math.Max(top_w, rects[i].Width);
                }
            }

            bool bFromLeft = top_x < 16 && top_w > 14 && rects[0].X > 10;


            return bFromLeft;
        }

        //Trace PathData to have the data.
        public void GenAnimatepath(ref GraphicsPath orig, ref List animates)
        {
            //alue   Meaning  PathTypes https://msdn.microsoft.com/en-us/library/system.drawing.drawing2d.graphicspath.pathtypes(v=vs.110).aspx
            //0      Indicates that the point is the start of a figure.
            //1      Indicates that the point is one of the two endpoints of a line.
            //3      Indicates that the point is an endpoint or control point of a cubic Bézier spline.
            //0x7    Masks all bits except for the three low-order bits, which indicate the point type.
            //0x20   Specifies that the point is a marker.
            //0x80   Specifies that the point is the last point in a closed subpath (figure).

            //Find The Top-Left Point

            GraphicsPath t = (GraphicsPath)orig.Clone();

            PointF[] points = orig.PathPoints;
            byte[] types = orig.PathTypes;

            bool isTi = IsTi(ref t);

            bool isLeftToRight = IsLeftToRight(ref t);

            //Transform to 64x64 is much easier
            Matrix translate = new Matrix();
            RectangleF b = t.GetBounds();
            float scale = 64.0f / Math.Max(b.Width, b.Height);

            //Translate by Top Left
            translate.Translate(-b.X * scale, -b.Y * scale);
            translate.Scale(scale, scale);
            t.Transform(translate);

            //Most Left Top Point
            //Most Top Points
            //MostBottom Points

            PointF start = new PointF();
            int start_index = 0;
            points = t.PathPoints;
            types = t.PathTypes;
            float x, y;
            if (isLeftToRight)
            {

                x = 64; y = 0;
                for (int i = 0; i < t.PointCount; i++)
                {
                    if (points[i].Y < 16 && points[i].X <= x)
                    {
                        x = points[i].X;
                        start_index = i;
                    }
                }
            }
            else if (isTi)
            {
                x = 0; y = 0;
                for (int i = 0; i < t.PointCount; i++)
                {
                    if (points[i].Y > y)
                    {
                        y = points[i].Y;
                        start_index = i;
                    }
                }
            }
            else
            {
                x = 0; y = 64;
                for (int i = 0; i < t.PointCount; i++)
                {
                    if (points[i].Y < y)
                    {
                        y = points[i].Y;
                        start_index = i;
                    }
                }

            }
            //Map to Original GraphicsPath
            start = orig.PathPoints[start_index];


            float length = 0;
            points = orig.PathPoints;
            types = orig.PathTypes;
            List distances = new List();

            //To Map from distances[0];
            distances.Add(0);
            for (int i = 0; i < orig.PointCount; i++)
            {
                int idx1 = (i + start_index) % orig.PointCount;
                int idx2 = (i + start_index + 1) % orig.PointCount;
                float dis = (float)Math.Sqrt((points[idx1].X - points[idx2].X) * (points[idx1].X - points[idx2].X) +
                           (points[idx1].Y - points[idx2].Y) * (points[idx1].Y - points[idx2].Y));

                distances.Add(dis);
                length += dis;
            }

            //Segment to 10
            int segments = Math.Min(10, orig.PointCount / 2) * 2;

            //Trace Forward,
            //Total len l,  trace from Start point for require_len/2, then trace ( total - requireed_len_) then all rest all, 
            //Generate Polygon   

            float steps = length / segments;

            //trace forward           
            float curDis = 0;
            int curIdx = 0;  //IDX in distances
            List idxList = new List();
            List distances2 = new List();
            for (int i = 1; i < segments; i++)
            {
                while (curDis < i * steps)
                {
                    curIdx++;
                    curDis += distances[curIdx];
                }
                distances2.Add(curDis);
                //Map back to Original index;
                idxList.Add(curIdx + start_index);
            }
            idxList.Add(start_index + orig.PointCount);


            //Generate Paths
            List newpoints = new List();
            for (int i = 0; i < segments / 2 - 1; i++)
            {
                //trace Forward
                newpoints.Clear();
                for (int j = start_index; j <= idxList[i]; j++)
                {
                    int idx = j % orig.PointCount;
                    newpoints.Add(points[idx]);
                }

                //trace forward in other side
                int sidx, eidx;
                sidx = idxList[idxList.Count - (i + 1) - 1];
                eidx = idxList[idxList.Count - 1];
                for (int j = sidx; j <= eidx; j++)
                {
                    int idx = j % orig.PointCount;
                    newpoints.Add(points[idx]);
                }

                GraphicsPath p = new GraphicsPath();
                p.AddPolygon(newpoints.ToArray());
                animates.Add(p);
            }
        }
    }

}