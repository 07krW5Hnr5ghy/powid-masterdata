package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.UserType;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface UserTypeRepositoryCustom {
    Page<UserType> searchForUserType(String userType,
                                     String user,
                                     String sort,
                                     String sortColumn,
                                     Integer pageNumber,
                                     Integer pageSize,
                                     Boolean status);
}
