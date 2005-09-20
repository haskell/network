/* Cut down from base/include/Typeable.h */
#ifndef TYPEABLE_H
#define TYPEABLE_H

#define INSTANCE_TYPEABLE0(tycon,tcname,str) \
tcname = mkTyCon str; \
instance Typeable tycon where { typeOf _ = mkTyConApp tcname [] }

#endif
