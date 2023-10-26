package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.ModuleType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface ModuleTypeRepositoryCustom {
    Page<ModuleType> searchForModuleType(Long idUserType,
                                         Long idModule,
                                         String sort,
                                         String sortColumn,
                                         Integer pageNumber,
                                         Integer pageSize);
}
