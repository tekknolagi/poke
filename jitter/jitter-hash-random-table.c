/* Jitter: random numbers for hashing.

   Copyright (C) 2017 Luca Saiu
   Written by Luca Saiu

   This file is part of Jitter.

   Jitter is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   Jitter is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with Jitter.  If not, see <http://www.gnu.org/licenses/>. */


#include "jitter.h"


/* These numbers are used in jitter-hash.c in a hashing function for string
   hash tables.

   Should these ever need to be generated again, this is a reasonable way to do
   it:

     wordsize=8
     comma=","
     for x in $(seq 0 255); do
       if test $x = 255; then
         comma=""
       fi
       echo "      (jitter_uint) 0x$(openssl rand $wordsize -hex)L$comma"
     done

   Notice that the result of casting to a narrower *unsigned* integer type in C
   is "guaranteed to be congruent to the source integer"; thanks to this I can
   define only 64-bit constants, without conditionalizing on the word size. */
#if JITTER_BYTES_PER_WORD > 8
#warning "The machine word size is bigger than 64 bits.  Weird."
#warning "You may want to make these constants wider."
#endif // #if JITTER_BYTES_PER_WORD > 8
const jitter_uint
jitter_hash_random_words [256]
  = {
      (jitter_uint) 0xd1bea6dc57bc2cbaL,
      (jitter_uint) 0x49ae8682c44e8b07L,
      (jitter_uint) 0x2d1e81420d7f5d2aL,
      (jitter_uint) 0x21084cea8bd81943L,
      (jitter_uint) 0xb228fc97b6624ec2L,
      (jitter_uint) 0x6d8eb8f5ab1c3b9cL,
      (jitter_uint) 0xe3a17263ec1517bdL,
      (jitter_uint) 0x0f6160201ed12c1aL,
      (jitter_uint) 0x67e2496139a32218L,
      (jitter_uint) 0xbf967825d398e859L,
      (jitter_uint) 0xcb5894f69a1457e6L,
      (jitter_uint) 0x42e86f44e5dcc002L,
      (jitter_uint) 0xe61b3393f80e3e10L,
      (jitter_uint) 0xb7e9b91745e8cb72L,
      (jitter_uint) 0x32573c413c43c7f0L,
      (jitter_uint) 0x97c4b40ac61d960dL,
      (jitter_uint) 0x910e977f19b7d36cL,
      (jitter_uint) 0x8167fe3df56cf1f2L,
      (jitter_uint) 0x3b268bba4d65bf00L,
      (jitter_uint) 0x820f8a0d45398c8eL,
      (jitter_uint) 0x6d20270951f0fc72L,
      (jitter_uint) 0x77c6be19d30cb242L,
      (jitter_uint) 0x2a7525df2238c4e2L,
      (jitter_uint) 0xf95c3a0e064c7f38L,
      (jitter_uint) 0x2d4760b2ad5e6b8fL,
      (jitter_uint) 0x70c2c1c1e29a7dcfL,
      (jitter_uint) 0x3c28ae952a132799L,
      (jitter_uint) 0xbc92a3036e8b4bb4L,
      (jitter_uint) 0x48fbe229f067aaf6L,
      (jitter_uint) 0x48446ea29fc5e398L,
      (jitter_uint) 0x83099118c4fba229L,
      (jitter_uint) 0x8ea820ad40e95627L,
      (jitter_uint) 0xf30834e28833c8e0L,
      (jitter_uint) 0x4c67d790cc8e873bL,
      (jitter_uint) 0x9644bcb98c45bf15L,
      (jitter_uint) 0x40764843121ab73aL,
      (jitter_uint) 0x98bdac5852fa89cdL,
      (jitter_uint) 0x73fbcc4b3e533fa2L,
      (jitter_uint) 0xd80699b592973810L,
      (jitter_uint) 0xa8838ff8d6df6bfaL,
      (jitter_uint) 0x5a47d18741b5dfa4L,
      (jitter_uint) 0x6178416c79ad6898L,
      (jitter_uint) 0xe530a4ffb0c36020L,
      (jitter_uint) 0xbdf261f1663e391bL,
      (jitter_uint) 0x0e68c3c9af7fdd8dL,
      (jitter_uint) 0x346c02ac3aabd1ffL,
      (jitter_uint) 0x8c16f71a0622385bL,
      (jitter_uint) 0x22213071e71a8b47L,
      (jitter_uint) 0x0fc058efcf46937aL,
      (jitter_uint) 0x72fce6143a7a6bc0L,
      (jitter_uint) 0xdd389a9aed0c0e6fL,
      (jitter_uint) 0x07c745a2b86eb59bL,
      (jitter_uint) 0x974ffb7b5c056045L,
      (jitter_uint) 0x235a46c729e7c7bdL,
      (jitter_uint) 0xfe1528c449ce52b1L,
      (jitter_uint) 0x77d563ad229f64d5L,
      (jitter_uint) 0xab9e8e879fbe79b4L,
      (jitter_uint) 0x9ab9d8a6654e30a1L,
      (jitter_uint) 0x7fe80afd88a8e516L,
      (jitter_uint) 0x83a719d7808430a3L,
      (jitter_uint) 0x092e63458c47c727L,
      (jitter_uint) 0xe15d18c7aae445eaL,
      (jitter_uint) 0x3cb3bf5a8171c9beL,
      (jitter_uint) 0x45dc201a8fa8b89aL,
      (jitter_uint) 0x27f0aba15e542790L,
      (jitter_uint) 0x5ed54100fc971bedL,
      (jitter_uint) 0xd29ae3d508cc4b63L,
      (jitter_uint) 0x2f459cb51718e9c6L,
      (jitter_uint) 0xc60861f337da8f05L,
      (jitter_uint) 0x7af17e2f8d6c7a1eL,
      (jitter_uint) 0xc73b2d3bdaba21acL,
      (jitter_uint) 0x9fa6ab0a3608516bL,
      (jitter_uint) 0x582161938088d7deL,
      (jitter_uint) 0xa5c32e2698edb4b3L,
      (jitter_uint) 0xebfcf4bdb03697d2L,
      (jitter_uint) 0x4e96a33858903f10L,
      (jitter_uint) 0xa0ac32d3d9578c8bL,
      (jitter_uint) 0xbeafc414e17bdd02L,
      (jitter_uint) 0x3ec169cd6253403fL,
      (jitter_uint) 0xed2cd6436a2146baL,
      (jitter_uint) 0x44eff35dc5361d4fL,
      (jitter_uint) 0xb0762c5f738a3245L,
      (jitter_uint) 0xe232254ef7f3928dL,
      (jitter_uint) 0xe7345bd3af634689L,
      (jitter_uint) 0x6ab70d922f9f70f8L,
      (jitter_uint) 0x21956f0fce0948e9L,
      (jitter_uint) 0xc5f3e2f59589a419L,
      (jitter_uint) 0xf9aef477dd2448efL,
      (jitter_uint) 0x890fb818f3528766L,
      (jitter_uint) 0x32b42013e07b8369L,
      (jitter_uint) 0x883650474b7d4383L,
      (jitter_uint) 0x5e098f472aba1591L,
      (jitter_uint) 0xa575a906e954c199L,
      (jitter_uint) 0x9417d5c25b9cad56L,
      (jitter_uint) 0x56bfd663d617baf2L,
      (jitter_uint) 0xa10fdeffc6206500L,
      (jitter_uint) 0xf16b722d544a0663L,
      (jitter_uint) 0x7ca6d83d876be0f9L,
      (jitter_uint) 0x36ddd0dae85ee6a6L,
      (jitter_uint) 0x1d80f55df2c4a1f0L,
      (jitter_uint) 0xecb6ef7eb1e4cc71L,
      (jitter_uint) 0xed28ea0e7f13f4ddL,
      (jitter_uint) 0x6f1a42afa7b6e3fcL,
      (jitter_uint) 0x4b4252a2b54176c8L,
      (jitter_uint) 0x37696ce4a1014272L,
      (jitter_uint) 0x35dc2df84c57ec60L,
      (jitter_uint) 0x0e145c96ef11c13aL,
      (jitter_uint) 0x7caea135322cac7cL,
      (jitter_uint) 0x66eaea4c0d774ca9L,
      (jitter_uint) 0x333d7e55f1d8eb19L,
      (jitter_uint) 0x6165dea38f450de1L,
      (jitter_uint) 0x30e4f4ffd89aa1a6L,
      (jitter_uint) 0xb28c6e06e1037c6dL,
      (jitter_uint) 0xc4fe10e83ba539bdL,
      (jitter_uint) 0xf0703a10c01989a8L,
      (jitter_uint) 0xe0f8d4d35c3f97cdL,
      (jitter_uint) 0xd20488669c97bcf9L,
      (jitter_uint) 0x409bda5abfcba914L,
      (jitter_uint) 0xa0c5993f975ba743L,
      (jitter_uint) 0x3c0a412432cf2f01L,
      (jitter_uint) 0xe59dc37a166e31d0L,
      (jitter_uint) 0x187e2e1b90cfdcedL,
      (jitter_uint) 0x564602169757e06eL,
      (jitter_uint) 0x0c3eac644d8f6ecaL,
      (jitter_uint) 0x301d9292db7cb54fL,
      (jitter_uint) 0x12bdc6886f15aaf3L,
      (jitter_uint) 0x8e6cfb343eb39c7eL,
      (jitter_uint) 0xa9c43c14a215eb7eL,
      (jitter_uint) 0x730a78d3936904b3L,
      (jitter_uint) 0xf722bcd05e5b5c92L,
      (jitter_uint) 0x7f4b05b301416b84L,
      (jitter_uint) 0x9950603f1f1fb6d5L,
      (jitter_uint) 0x171030c392db1f53L,
      (jitter_uint) 0x6a5b5a6b670d4012L,
      (jitter_uint) 0xd5b2c41d19647950L,
      (jitter_uint) 0x5271ff1bd8135106L,
      (jitter_uint) 0x46b48b9ca8267adeL,
      (jitter_uint) 0x26fd1cb65b990bedL,
      (jitter_uint) 0x20931a7e034f086cL,
      (jitter_uint) 0xbd60d0b00b64e224L,
      (jitter_uint) 0x3e4e14b6f20ce7f9L,
      (jitter_uint) 0x888eef49ae98b685L,
      (jitter_uint) 0x985eb33b9dbb1024L,
      (jitter_uint) 0xf12f45919c75b0d6L,
      (jitter_uint) 0xb291f1968dc476e6L,
      (jitter_uint) 0x25197b6b107f1b1aL,
      (jitter_uint) 0x6356a2a94e50c663L,
      (jitter_uint) 0x5ea5d89040a7291eL,
      (jitter_uint) 0xde5d5e773e77b472L,
      (jitter_uint) 0x3498daa493042393L,
      (jitter_uint) 0x9a60c9a63ca6d264L,
      (jitter_uint) 0x4ea5343c05e35acaL,
      (jitter_uint) 0x46f73b1ff86e6e9eL,
      (jitter_uint) 0x9b52d140249224c8L,
      (jitter_uint) 0x65d4e7cba7974973L,
      (jitter_uint) 0xec48ef4b4e918a9aL,
      (jitter_uint) 0xb192b8903859d648L,
      (jitter_uint) 0x42f5065cbe4b49f2L,
      (jitter_uint) 0x735823061daf5288L,
      (jitter_uint) 0x045623e26f650956L,
      (jitter_uint) 0xd04700bea83af662L,
      (jitter_uint) 0x69ee05e7c3ae7247L,
      (jitter_uint) 0x1a1a285f2c3f2488L,
      (jitter_uint) 0xde824d223d74e1a7L,
      (jitter_uint) 0x3cd251bb0ae1e6e5L,
      (jitter_uint) 0x2d85e46f32aaa58eL,
      (jitter_uint) 0x285275905a518e3aL,
      (jitter_uint) 0x4cbb919dea9d5215L,
      (jitter_uint) 0xdcca00b4e61460aaL,
      (jitter_uint) 0xa3701f9932a6f20fL,
      (jitter_uint) 0x01d600a430ef4880L,
      (jitter_uint) 0x4e158bf9b5f3f1c8L,
      (jitter_uint) 0x957fb216d4e84485L,
      (jitter_uint) 0xb18525b4c7648d40L,
      (jitter_uint) 0x954b8c1755ad5700L,
      (jitter_uint) 0xcbcf52ae91535961L,
      (jitter_uint) 0xc06beac0e2afb0e7L,
      (jitter_uint) 0x5887d291d728eee2L,
      (jitter_uint) 0xcefd3871692f6cb8L,
      (jitter_uint) 0xc8822cbfb69dc5ffL,
      (jitter_uint) 0xaf4c7b29ba97368aL,
      (jitter_uint) 0x4292db232a3bd2e2L,
      (jitter_uint) 0xda114ee566c91ff2L,
      (jitter_uint) 0x0855abf4a0b394ceL,
      (jitter_uint) 0x8b6506cae34c1dfbL,
      (jitter_uint) 0x715ca5fc749abf4fL,
      (jitter_uint) 0xe8a6271e11fdc18aL,
      (jitter_uint) 0x239ef85c17a5da71L,
      (jitter_uint) 0x5b2223caeed60530L,
      (jitter_uint) 0xe878c97aba7923eeL,
      (jitter_uint) 0x844955495413762bL,
      (jitter_uint) 0x4cde3a38359ea70eL,
      (jitter_uint) 0x7c85e21247a73d23L,
      (jitter_uint) 0xd0d5edfb7f385648L,
      (jitter_uint) 0x010fe7975f1a3889L,
      (jitter_uint) 0xec4419d6125d2ed4L,
      (jitter_uint) 0xf3bac2b7f0b9057aL,
      (jitter_uint) 0x425e174f44d83812L,
      (jitter_uint) 0x4c0efb6083ba9b65L,
      (jitter_uint) 0x21c126694209522eL,
      (jitter_uint) 0x29726b8abb598e78L,
      (jitter_uint) 0x0922f581fb5fb71dL,
      (jitter_uint) 0xdd2a026e87c65269L,
      (jitter_uint) 0x8e5089f87f7755b9L,
      (jitter_uint) 0xd7d7946d1a0e5872L,
      (jitter_uint) 0xadd46393071fba41L,
      (jitter_uint) 0x2b3864c46bad7586L,
      (jitter_uint) 0x4a6f3fa907507c53L,
      (jitter_uint) 0xaeb60c442482b733L,
      (jitter_uint) 0xe4a5760841676af6L,
      (jitter_uint) 0xee59b111017d91fdL,
      (jitter_uint) 0x68c4587c527898b3L,
      (jitter_uint) 0x508cdd42a2079449L,
      (jitter_uint) 0x45211f01b9d1d19cL,
      (jitter_uint) 0xf737572cdf836a25L,
      (jitter_uint) 0x6fcb87d36314fa2dL,
      (jitter_uint) 0xe601a3e425b8d566L,
      (jitter_uint) 0x60fdd72741674b2eL,
      (jitter_uint) 0x9e2660a8d698b20eL,
      (jitter_uint) 0xe70898e428c32a93L,
      (jitter_uint) 0xfc2c4b6e19c8a50bL,
      (jitter_uint) 0x4c06d276590f66f9L,
      (jitter_uint) 0x3c5e6dfe6766db60L,
      (jitter_uint) 0x212779805e4ed29fL,
      (jitter_uint) 0x8cff214068d2c949L,
      (jitter_uint) 0xfb6655b50733505fL,
      (jitter_uint) 0xb1387f0007fc7419L,
      (jitter_uint) 0x37385a023be22a36L,
      (jitter_uint) 0x8851ec5e720af28dL,
      (jitter_uint) 0x80bc95b23b41b931L,
      (jitter_uint) 0xfedf4d61fad2f0b1L,
      (jitter_uint) 0x143a8aa76a09ff3fL,
      (jitter_uint) 0xb2ef23d92941c8b3L,
      (jitter_uint) 0x6fa706477e8396d4L,
      (jitter_uint) 0xe17463e5a445786cL,
      (jitter_uint) 0xf2258d7294e6e0b6L,
      (jitter_uint) 0x516580e33818ff38L,
      (jitter_uint) 0x678b1dd4571d16b0L,
      (jitter_uint) 0x68478d74a8f4800cL,
      (jitter_uint) 0xca766b92134136b9L,
      (jitter_uint) 0x2f6311269bbbe919L,
      (jitter_uint) 0x1659902aa301a4e3L,
      (jitter_uint) 0x6a653261adeb5458L,
      (jitter_uint) 0x71dc359d59dfedc0L,
      (jitter_uint) 0xfa195781a3b97ab7L,
      (jitter_uint) 0x33386ead50bbafadL,
      (jitter_uint) 0x105d138c73428999L,
      (jitter_uint) 0x21ebc7e1e953ba84L,
      (jitter_uint) 0xf74a48d32740562aL,
      (jitter_uint) 0x888e08bcdd4d05dfL,
      (jitter_uint) 0xe451243a3cd7adafL,
      (jitter_uint) 0xc8dc30ed027b0b07L,
      (jitter_uint) 0x457a8cf3a12bb944L,
      (jitter_uint) 0xbacc5ae483ffa81cL,
      (jitter_uint) 0xa6687f8b6a14f00fL,
      (jitter_uint) 0x395be2a8593b6d5eL
    };
