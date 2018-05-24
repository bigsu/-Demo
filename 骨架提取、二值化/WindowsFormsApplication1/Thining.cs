
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using System.Drawing;
using System.Drawing.Imaging;
using System.Runtime.InteropServices;
/* --------------------------------------------------------
* 作者：livezingy
*
* 博客：http://www.livezingy.com
*
* 开发环境：
* Visual Studio V2012
* .NET Framework 4.5
*
* 版本历史：
* V1.0 2015年04月29日
* 为图像瘦身
--------------------------------------------------------- */
namespace WindowsFormsApplication1
{
    public static class Thining
    {
        //调用此函数即可实现提取图像骨架
        public static Bitmap getThinPicture(string imageSrcPath, string imageDestPath)
        {
            Bitmap bmp = new Bitmap(imageSrcPath);
            int Threshold = 0;
            Byte[,] m_SourceImage = ToBinaryArray(bmp, out Threshold);
            Byte[,] m_DesImage =ThinPicture(m_SourceImage);
            return BinaryArrayToBinaryBitmap(m_DesImage);
           
            //Bitmap bmpThin = BinaryArrayToBinaryBitmap(m_DesImage);
            // bmpThin.Save(imageDestPath, System.Drawing.Imaging.ImageFormat.Jpeg);
        }
        public static int B(Byte[,] picture, int x, int y)
        {
            return picture[x, y - 1] + picture[x + 1, y - 1] + picture[x + 1, y] + picture[x + 1, y + 1] +
            picture[x, y + 1] + picture[x - 1, y + 1] + picture[x - 1, y] + picture[x - 1, y - 1];
        }
        public static int A(Byte[,] picture, int x, int y)
        {
            int counter = 0;
            if ((picture[x, y - 1] == 0) && (picture[x + 1, y - 1] == 1))
            {
                counter++;
            }
            if ((picture[x + 1, y - 1] == 0) && (picture[x + 1, y] == 1))
            {
                counter++;
            }
            if ((picture[x + 1, y] == 0) && (picture[x + 1, y + 1] == 1))
            {
                counter++;
            }
            if ((picture[x + 1, y + 1] == 0) && (picture[x, y + 1] == 1))
            {
                counter++;
            }
            if ((picture[x, y + 1] == 0) && (picture[x - 1, y + 1] == 1))
            {
                counter++;
            }
            if ((picture[x - 1, y + 1] == 0) && (picture[x - 1, y] == 1))
            {
                counter++;
            }
            if ((picture[x - 1, y] == 0) && (picture[x - 1, y - 1] == 1))
            {
                counter++;
            }
            if ((picture[x - 1, y - 1] == 0) && (picture[x, y - 1] == 1))
            {
                counter++;
            }
            return counter;
        }
        public static Byte[,] ThinPicture(Byte[,] newPicture)
        {
            Byte[,] picture = new Byte[newPicture.GetLength(0) + 2, newPicture.GetLength(1) + 2];
            Byte[,] pictureToRemove = new Byte[newPicture.GetLength(0) + 2, newPicture.GetLength(1) + 2];
            bool hasChanged;
            for (int i = 0; i < picture.GetLength(1); i++)
            {
                for (int j = 0; j < picture.GetLength(0); j++)
                {
                    picture[j, i] = 255;
                    pictureToRemove[j, i] = 0;
                }
            }
            for (int i = 0; i < newPicture.GetLength(1); i++)
            {
                for (int j = 0; j < newPicture.GetLength(0); j++)
                {
                    picture[j + 1, i + 1] = newPicture[j, i];
                }
            }
            for (int i = 0; i < picture.GetLength(1); i++)
            {
                for (int j = 0; j < picture.GetLength(0); j++)
                {
                    picture[j, i] = picture[j, i] == 0 ? picture[j, i] = 1 : picture[j, i] = 0;
                }
            }
            do
            {
                hasChanged = false;
                for (int i = 0; i < newPicture.GetLength(1); i++)
                {
                    for (int j = 0; j < newPicture.GetLength(0); j++)
                    {
                        if ((picture[j, i] == 1) && (2 <= B(picture, j, i)) && (B(picture, j, i) <= 6) && (A(picture, j, i) == 1)
                       &&
                        (picture[j, i - 1] * picture[j + 1, i] * picture[j, i + 1] == 0) &&
                        (picture[j + 1, i] * picture[j, i + 1] * picture[j - 1, i] == 0))
                        {
                            pictureToRemove[j, i] = 1;
                            hasChanged = true;
                        }
                    }
                }
                for (int i = 0; i < newPicture.GetLength(1); i++)
                {
                    for (int j = 0; j < newPicture.GetLength(0); j++)
                    {
                        if (pictureToRemove[j, i] == 1)
                        {
                            picture[j, i] = 0;
                            pictureToRemove[j, i] = 0;
                        }
                    }
                }
                for (int i = 0; i < newPicture.GetLength(1); i++)
                {
                    for (int j = 0; j < newPicture.GetLength(0); j++)
                    {
                        if ((picture[j, i] == 1) && (2 <= B(picture, j, i)) && (B(picture, j, i) <= 6) &&
                        (A(picture, j, i) == 1) &&
                        (picture[j, i - 1] * picture[j + 1, i] * picture[j - 1, i] == 0) &&
                        (picture[j, i - 1] * picture[j, i + 1] * picture[j - 1, i] == 0))
                        {
                            pictureToRemove[j, i] = 1;
                            hasChanged = true;
                        }
                    }
                }
                for (int i = 0; i < newPicture.GetLength(1); i++)
                {
                    for (int j = 0; j < newPicture.GetLength(0); j++)
                    {
                        if (pictureToRemove[j, i] == 1)
                        {
                            picture[j, i] = 0;
                            pictureToRemove[j, i] = 0;
                        }
                    }
                }
            } while (hasChanged);
            for (int i = 0; i < newPicture.GetLength(1); i++)
            {
                for (int j = 0; j < newPicture.GetLength(0); j++)
                {
                    if ((picture[j, i] == 1) &&
                    (((picture[j, i - 1] * picture[j + 1, i] == 1) && (picture[j - 1, i + 1] != 1)) || ((picture[j + 1, i] * picture[j, i + 1]
                   == 1) && (picture[j - 1, i - 1] != 1)) || //Небольшая модификцаия алгоритма для ещё большего утоньшения
                    ((picture[j, i + 1] * picture[j - 1, i] == 1) && (picture[j + 1, i - 1] != 1)) || ((picture[j, i - 1] * picture[j - 1, i] ==
                   1) && (picture[j + 1, i + 1] != 1))))
                    {
                        picture[j, i] = 0;
                    }
                }
            }
            for (int i = 0; i < picture.GetLength(1); i++)
            {
                for (int j = 0; j < picture.GetLength(0); j++)
                {
                    // picture[j, i] = picture[j, i] == 0 ? 255 : 0;
                    if (0 == picture[j, i])
                    {
                        picture[j, i] = 255;
                    }
                    else
                    {
                        picture[j, i] = 0;
                    }
                }
            }
            Byte[,] outPicture = new Byte[newPicture.GetLength(0), newPicture.GetLength(1)];
            for (int i = 0; i < newPicture.GetLength(1); i++)
            {
                for (int j = 0; j < newPicture.GetLength(0); j++)
                {
                    outPicture[j, i] = picture[j + 1, i + 1];
                }
            }
            return outPicture;
        }
        /// <summary>
        /// 全局阈值图像二值化
        /// </summary>
        /// <param name="bmp">原始图像</param>
        /// <param name="method">二值化方法</param>
        /// <param name="threshold">输出：全局阈值</param>
        /// <returns>二值化后的图像数组</returns>
        public static Byte[,] ToBinaryArray(this Bitmap bmp, out Int32 threshold)
        { // 位图转换为灰度数组
            Byte[,] GrayArray = bmp.ToGrayArray();
            // 计算全局阈值
            threshold = OtsuThreshold(GrayArray);
            // 根据阈值进行二值化
            Int32 PixelHeight = bmp.Height;
            Int32 PixelWidth = bmp.Width;
            Byte[,] BinaryArray = new Byte[PixelHeight, PixelWidth];
            for (Int32 i = 0; i < PixelHeight; i++)
            {
                for (Int32 j = 0; j < PixelWidth; j++)
                {
                    BinaryArray[i, j] = Convert.ToByte((GrayArray[i, j] > threshold) ? 255 : 0);
                }
            }
            return BinaryArray;
        }
        /// <summary>
        /// 将位图转换为灰度数组（256级灰度）
        /// </summary>
        /// <param name="bmp">原始位图</param>
        /// <returns>灰度数组</returns>
        public static Byte[,] ToGrayArray(this Bitmap bmp)
        {
            Int32 PixelHeight = bmp.Height; // 图像高度
            Int32 PixelWidth = bmp.Width; // 图像宽度
            Int32 Stride = ((PixelWidth * 3 + 3) >> 2) << 2; // 跨距宽度
            Byte[] Pixels = new Byte[PixelHeight * Stride];
            // 锁定位图到系统内存
            BitmapData bmpData = bmp.LockBits(new Rectangle(0, 0, PixelWidth, PixelHeight), ImageLockMode.ReadOnly,
           PixelFormat.Format24bppRgb);
            Marshal.Copy(bmpData.Scan0, Pixels, 0, Pixels.Length); // 从非托管内存拷贝数据到托管内存
            bmp.UnlockBits(bmpData); // 从系统内存解锁位图
            // 将像素数据转换为灰度数组
            Byte[,] GrayArray = new Byte[PixelHeight, PixelWidth];
            for (Int32 i = 0; i < PixelHeight; i++)
            {
                Int32 Index = i * Stride;
                for (Int32 j = 0; j < PixelWidth; j++)
                {
                    GrayArray[i, j] = Convert.ToByte((Pixels[Index + 2] * 19595 + Pixels[Index + 1] * 38469 + Pixels[Index] * 7471
                   + 32768) >> 16);
                    Index += 3;
                }
            }
            return GrayArray;
        }
        /// <summary>
        /// 大津法计算阈值
        /// </summary>
        /// <param name="grayArray">灰度数组</param>
        /// <returns>二值化阈值</returns>
        public static Int32 OtsuThreshold(Byte[,] grayArray)
        { // 建立统计直方图
            Int32[] Histogram = new Int32[256];
            Array.Clear(Histogram, 0, 256); // 初始化
            foreach (Byte b in grayArray)
            {
                Histogram[b]++; // 统计直方图
            }
            // 总的质量矩和图像点数
            Int32 SumC = grayArray.Length; // 总的图像点数
            Double SumU = 0; // 双精度避免方差运算中数据溢出
            for (Int32 i = 1; i < 256; i++)
            {
                SumU += i * Histogram[i]; // 总的质量矩
            }
            // 灰度区间
            Int32 MinGrayLevel = Array.FindIndex(Histogram, NonZero); // 最小灰度值
            Int32 MaxGrayLevel = Array.FindLastIndex(Histogram, NonZero); // 最大灰度值
            // 计算最大类间方差
            Int32 Threshold = MinGrayLevel;
            Double MaxVariance = 0.0; // 初始最大方差
            Double U0 = 0; // 初始目标质量矩
            Int32 C0 = 0; // 初始目标点数
            for (Int32 i = MinGrayLevel; i < MaxGrayLevel; i++)
            {
                if (Histogram[i] == 0) continue;
                // 目标的质量矩和点数
                U0 += i * Histogram[i];
                C0 += Histogram[i];
                // 计算目标和背景的类间方差
                Double Diference = U0 * SumC - SumU * C0;
                Double Variance = Diference * Diference / C0 / (SumC - C0); // 方差
                if (Variance > MaxVariance)
                {
                    MaxVariance = Variance;
                    Threshold = i;
                }
            }
            // 返回类间方差最大阈值
            return Threshold;
        }
        /// <summary>
        /// 检测非零值
        /// </summary>
        /// <param name="value">要检测的数值</param>
        /// <returns>
        /// true：非零
        /// false：零
        /// </returns>
        private static Boolean NonZero(Int32 value)
        {
            return (value != 0) ? true : false;
        }
        /// <summary>
        /// 将二值化数组转换为二值化图像
        /// </summary>
        /// <param name="binaryArray">二值化数组</param>
        /// <returns>二值化图像</returns>
        public static Bitmap BinaryArrayToBinaryBitmap(Byte[,] binaryArray)
        { // 将二值化数组转换为二值化数据
            Int32 PixelHeight = binaryArray.GetLength(0);
            Int32 PixelWidth = binaryArray.GetLength(1);
            Int32 Stride = ((PixelWidth + 31) >> 5) << 2;
            Byte[] Pixels = new Byte[PixelHeight * Stride];
            for (Int32 i = 0; i < PixelHeight; i++)
            {
                Int32 Base = i * Stride;
                for (Int32 j = 0; j < PixelWidth; j++)
                {
                    if (binaryArray[i, j] != 0)
                    {
                        Pixels[Base + (j >> 3)] |= Convert.ToByte(0x80 >> (j & 0x7));
                    }
                }
            }
            // 创建黑白图像
            Bitmap BinaryBmp = new Bitmap(PixelWidth, PixelHeight, PixelFormat.Format1bppIndexed);
            // 设置调色表
            ColorPalette cp = BinaryBmp.Palette;
            cp.Entries[0] = Color.Black; // 黑色
            cp.Entries[1] = Color.White; // 白色
            BinaryBmp.Palette = cp;
            // 设置位图图像特性
            BitmapData BinaryBmpData = BinaryBmp.LockBits(new Rectangle(0, 0, PixelWidth, PixelHeight),
           ImageLockMode.WriteOnly, PixelFormat.Format1bppIndexed);
            Marshal.Copy(Pixels, 0, BinaryBmpData.Scan0, Pixels.Length);
            BinaryBmp.UnlockBits(BinaryBmpData);
            return BinaryBmp;
        }
    }
}