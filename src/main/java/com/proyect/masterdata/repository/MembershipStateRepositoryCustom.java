package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.MembershipState;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface MembershipStateRepositoryCustom {
    Page<MembershipState> searchForMembershipState(
            String name,
            String sort,
            String sortColumn,
            Integer pageNumber,
            Integer pageSize,
            Boolean status
    );
}
