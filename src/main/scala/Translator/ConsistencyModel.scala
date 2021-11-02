/*
 *
 *  Copyright (c) 2021. Mark Grechanik and Lone Star Consulting, Inc. All rights reserved.
 *
 *   Unless required by applicable law or agreed to in writing, software distributed under
 *   the License is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND,
 *   either express or implied.  See the License for the specific language governing permissions and limitations under the License.
 *
 */

package Translator

import Translator.SlanAbstractions.SlanConstruct

enum ConsistencyModel extends SlanConstruct :
  case STRICT extends ConsistencyModel
  case SEQUENTIAL extends ConsistencyModel
  case CAUSAL extends ConsistencyModel
  case PROCESSOR extends ConsistencyModel
  case PRAM extends ConsistencyModel
  case FIFO extends ConsistencyModel
  case CACHE extends ConsistencyModel
  case SLOW extends ConsistencyModel
  case RELEASE extends ConsistencyModel
  case EVENTUAL extends ConsistencyModel

