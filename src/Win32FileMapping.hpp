#include "types.hpp"
#include "Error.hpp"

// #define WIN32_MEAN_AND_LEAN
// #define min(a,b)
// #define max(a,b)
// #undef min
// #undef max

#define WIN32_MEAN_AND_LEAN
#include <windows.h>

// undefine min/max macros

#ifdef min
#  undef min
#endif
#ifdef max
#  undef max
#endif
#ifndef NOMINMAX
#  define NOMINMAX
#endif

namespace ff {
  
  // forward declarations
  class Win32FileMapping;
  class Win32FileSection;

  /** file mapping class */
  class Win32FileMapping
  {
  public:
  
    /** constructor, open file mapping on file given by native platform path */
    Win32FileMapping(const char* path, fsize_t size=0, bool readonly=false);
    
    /** destructor */
    ~Win32FileMapping();
  
    /** map section from file into memory */
    Win32FileSection* mapSection(foff_t offs, msize_t size, void* baseaddr=0);
  
    /** remap section to new offset and size */
    void remapSection(Win32FileSection& section, foff_t offs, msize_t size, void* baseaddr=0);
    
    /** get file size */
    inline fsize_t size() const { return _size; }
  
    /** get error status */
    inline Error getError() const { return _error; }
  
    /** get system page size */
    static msize_t getPageSize();
    
  private:
    HANDLE _fileHandle;
    HANDLE _viewHandle;
    fsize_t _size;
    Error   _error;
    bool    _readonly;
  };

  
  /** file section class represents a unit in virtual memory address space */
  class Win32FileSection
  {
  public:
  
    /** constructor */
    Win32FileSection(HANDLE viewHandle, foff_t offs, msize_t size, void* addr, bool readonly=false);
    
    /** destructor */
    ~Win32FileSection();
  
    /** reset file section to offset offs and size size */
    void  reset(foff_t offs, msize_t size, void* baseaddr=0);
  
    /** check if range at offset offs with length size is completely in section */
    inline bool checkRangeInSection(foff_t offs, msize_t size) const
    {
      return ( (_offset <= offs) && ( static_cast<foff_t>(offs+size) < _end ) );
    }
  
    /** check if byte at offset off is contained in section */
    inline bool  checkOffsetInSection(foff_t offs) const
    {
      return ( (_offset <= offs) && ( offs < _end ) );
    }
  
    /** obtain address pointer of offset offs */
    inline void* getPointer(foff_t offset)
    {
      foff_t disp = offset - _offset;
      return (void*) ( ( (unsigned char*)_addr  ) + disp );
    }
    
  private:
    HANDLE  _viewHandle;
    bool    _readonly;
    foff_t  _offset;
    foff_t  _end;
    msize_t _size;
    void*   _addr;
  };


}
