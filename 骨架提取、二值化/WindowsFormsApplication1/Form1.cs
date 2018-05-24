using System;
using System.Drawing;
using System.Drawing.Drawing2D;
using System.Drawing.Imaging;
using System.Windows.Forms;
using Emgu.CV;
using Emgu.CV.Structure;

namespace WindowsFormsApplication1
{
    public partial class Form1 : Form
    {
        private Point startPoint;
        private Point endPoint;
        private bool isDrawing;
        private Bitmap memoryBuffer;

        public Form1()
        {
            InitializeComponent();
        }
        private void Form1_Load(object sender, EventArgs e)
        {
            // pictureBox1.SizeMode = PictureBoxSizeMode.Zoom;
            pictureBox2.SizeMode = PictureBoxSizeMode.Zoom;
        }

        #region 按钮

        //打开图片
        private void button3_Click(object sender, EventArgs e)
        {
            using (OpenFileDialog ofd = new OpenFileDialog())
            {
                ofd.Filter = "图像文件|*.jpeg;*.gif;*.jpg;*.png;*.bmp;*.ico;|所有文件|*.*";
                if (ofd.ShowDialog() == DialogResult.OK)
                {
                    pictureBox1.SizeMode = PictureBoxSizeMode.Normal;
                    pictureBox1.Width = 477;
                    pictureBox1.Height = 366;
                    textBox1.Text = ofd.FileName;
                    Image imgSource = Image.FromFile(textBox1.Text);
                    pictureBox1.Image = ResetPictureBox(imgSource, pictureBox1);
                    pictureBox1.Paint += pictureBox1_Paint;
                    pictureBox1.MouseMove += pictureBox1_MouseMove;
                    pictureBox1.MouseUp += pictureBox1_MouseUp;
                    pictureBox1.MouseDown += pictureBox1_MouseDown;
                    label1.Text = "原图" + imgSource.Width + "," + imgSource.Height + "*****控件" + pictureBox1.Width +
                                  "," + pictureBox1.Height;
                }
            }
        }


        //获取骨架
        private void button1_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(textBox1.Text))
            {
                Bitmap bm = Thining.getThinPicture(textBox1.Text, "");
                pictureBox2.Image = bm;
            }
        }

        //二值化
        private void button2_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(textBox1.Text))
            {
                pictureBox2.Image = ImageClass.ConvertTo1Bpp1((Bitmap)Image.FromFile(textBox1.Text));
            }
        }


        //获取轮廓
        private void button4_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(textBox1.Text))
            {
                Image<Gray, byte> src1 = new Image<Gray, byte>(textBox1.Text);
                Image<Bgr, byte> src2 = new Image<Bgr, byte>(textBox1.Text);
                Bitmap img = ImageClass.GetOutline(src1, ref src2);
                pictureBox1.Image = src2.ToBitmap();
                pictureBox1.SizeMode = PictureBoxSizeMode.Zoom;
                pictureBox2.Image = img;
            }
        }

        //灰度化
        private void button5_Click(object sender, EventArgs e)
        {
            if (!string.IsNullOrEmpty(textBox1.Text))
            {
                pictureBox2.Image = ImageClass.ToGray((Bitmap)Image.FromFile(textBox1.Text));
            }
        }

        #endregion



        #region pictureBox事件

        //图像选择
        Point start; //画框的起始点
        Point end;//画框的结束点
        bool blnDraw;//判断是否绘制
        Rectangle rect;
        private void pictureBox1_MouseDown(object sender, MouseEventArgs e)
        {
            start = e.Location;
            Invalidate();
            blnDraw = true;
        }

        private void pictureBox1_MouseMove(object sender, MouseEventArgs e)
        {
            if (blnDraw)
            {
                if (e.Button != MouseButtons.Left)//判断是否按下左键
                    return;
                Point tempEndPoint = e.Location; //记录框的位置和大小
                rect.Location = new Point(
                Math.Min(start.X, tempEndPoint.X),
                Math.Min(start.Y, tempEndPoint.Y));
                rect.Size = new Size(
                Math.Abs(start.X - tempEndPoint.X),
                Math.Abs(start.Y - tempEndPoint.Y));
                pictureBox1.Invalidate();
            }
        }

        private void pictureBox1_MouseUp(object sender, MouseEventArgs e)
        {
            blnDraw = false; //结束绘制
            if (rect.Width > 0 && rect.Height > 0)
            {
                Bitmap b = AcquireRectangleImage(Image.FromFile(textBox1.Text), rect);
                pictureBox2.Image = b;
            }
        }


        private void pictureBox1_Paint(object sender, PaintEventArgs e)
        {
            if (blnDraw)
            {
                if (pictureBox1.Image != null)
                {
                    if (rect.Width > 0 && rect.Height > 0)
                    {
                        e.Graphics.DrawRectangle(new Pen(Color.Red, 1), rect);//重新绘制颜色为红色
                    }
                }
            }

        }
        #endregion


        /// <summary>
        /// 截取图像的矩形区域
        /// </summary>
        /// <param name="source">源图像对应picturebox1</param>
        /// <param name="rect">矩形区域，如上初始化的rect</param>
        /// <returns>矩形区域的图像</returns>
        public Bitmap AcquireRectangleImage(Image source, Rectangle rectangle)
        {
            rectangle.X = (int)Math.Ceiling((double)source.Width / pictureBox1.Width * rectangle.X);
            rectangle.Y = (int)Math.Ceiling((double)source.Width / pictureBox1.Width * rectangle.Y);
            rectangle.Width = (int)Math.Ceiling((double)source.Width / pictureBox1.Width * rectangle.Width);
            rectangle.Height = (int)Math.Ceiling((double)source.Width / pictureBox1.Width * rectangle.Height);

            if (rectangle.IsEmpty) return null;
            Bitmap bmSmall = new Bitmap(rectangle.Width, rectangle.Height, PixelFormat.Format32bppRgb);
            //Bitmap bmSmall = new Bitmap(rect.Width, rect.Height, source.PixelFormat);
            using (Graphics grSmall = Graphics.FromImage(bmSmall))
            {
                grSmall.DrawImage(source,
                                  new Rectangle(0, 0, bmSmall.Width, bmSmall.Height),
                                  rectangle,
                                  GraphicsUnit.Pixel);
                grSmall.Dispose();
            }
            return bmSmall;
        }


        //缩放图片
        private Bitmap ResetPictureBox(Image imgSource, PictureBox pb)
        {
            int pbWidth = pb.Width;//新图宽
            int pbHeight = pb.Height;//新图高

            int sW, sH;
            // 按比例缩放           
            int sWidth = imgSource.Width;//原图宽
            int sHeight = imgSource.Height;//原图高
            if (sHeight > pbHeight || sWidth > pbWidth)
            {
                if ((sWidth * pbHeight) > (sHeight * pbWidth))
                {
                    sW = pbWidth;
                    sH = (pbWidth * sHeight) / sWidth;
                }
                else
                {
                    sH = pbHeight;
                    sW = (sWidth * pbHeight) / sHeight;
                }
            }
            else
            {
                sW = sWidth;
                sH = sHeight;
            }

            Bitmap outBmp = new Bitmap(sW, sHeight);
            Graphics g = Graphics.FromImage(outBmp);
            g.Clear(Color.Transparent);
            // 设置画布的描绘质量         
            g.CompositingQuality = CompositingQuality.HighQuality;
            g.SmoothingMode = SmoothingMode.HighQuality;
            g.InterpolationMode = InterpolationMode.HighQualityBicubic;
            g.DrawImage(imgSource, new Rectangle(0, 0, sW, sH), 0, 0, imgSource.Width,
                imgSource.Height, GraphicsUnit.Pixel);
            g.Dispose();
            pb.Height = sH;
            pb.Width = sW;
            return outBmp;
        }
    }
}
