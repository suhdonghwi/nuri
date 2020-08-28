# 누리 사용해보기

## 실행 파일

### 빌드된 바이너리

[누리 릴리즈](https://github.com/suhdonghwi/nuri/releases)에 들어가서 운영체제에 맞는 바이너리를 받습니다.

### 직접 빌드

빌드에 필요한 의존성은 다음과 같습니다.

- [pypy@44f2564](https://github.com/mozillazg/pypy/tree/44f2564f00230d1fb80c0ff1b36ce18bebe938db)
- [stack](https://docs.haskellstack.org/en/stable/README/) (>= 2.0.0)
- python 혹은 pypy **2.x**
- git

누리 소스 코드 폴더의 루트 경로에서 다음 명령을 실행합니다. python을 기준으로 작성되었습니다.

```bash
$ git pull --recurse-submodules
$ python (pypy 경로)/rpython/bin/rpython --opt=(최적화 설정) ./haneul/src/target.py
$ stack build

# 리눅스, 맥의 경우
$ mv target-c haneul
$ chmod +x haneul

# 윈도우의 경우
$ mv target-c.exe haneul.exe
```

최적화 수준은 0, 1, 2, 3, jit, size, mem 중 하나를 선택하면 됩니다. 하늘은 JIT을 지원합니다.

이렇게 하면 누리 소스코드 폴더에 haneul 이라는 이름으로 하늘 실행 파일이 생성되고, 누리의 실행 파일이 빌드됩니다. 누리의 실행 파일은 `stack install` 명령을 통해서 적절한 위치에 설치할 수 있습니다. 누리의 실행 파일과 하늘의 실행 파일을 같은 경로에 두고 실행하면 됩니다. (혹은 `--haneul` 명령줄 인수 지정)

## 실행

```
누리 - 함수형 한글 프로그래밍 언어 0.1.0

사용법:
  nuri --help | -h
  nuri --version | -v
  nuri <파일명> [--debug | -d] [--haneul=<path>]
옵션:",
  -h, --help        도움말을 출력합니다.
  -v, --version     누리 실행기의 버전을 출력합니다.
  -d, --debug       디버그용 출력을 활성화합니다.
  --haneul=<path>   하늘 가상머신 실행 파일의 경로를 설정합니다.
                    [기본 : './haneul']
```

윈도우의 경우 EUC-KR 코드페이지 환경에서 한글 출력이 제대로 되지 않는다는 문제점이 있습니다. 추후 수정할 예정이지만, 현재는 임시방편으로 누리 실행 전 다음 명령을 실행하면 됩니다.

```
$ chcp 65001
```

기본적인 프로그램을 작성하기 위한 유용한 함수들은 배포 압축 파일에 동봉되어있는 (혹은 [여기](https://github.com/suhdonghwi/nuri/blob/master/examples/유용한%20함수들.nuri)) `유용한 함수.nuri`에 있습니다.

```
꾸러미 "유용한 함수.nuri"
```

작성하고 있는 코드 파일과 같은 경로에 `유용한 함수.nuri`를 위치시키고, 위와 같이 코드에 파일을 포함해서 사용할 수 있습니다.

