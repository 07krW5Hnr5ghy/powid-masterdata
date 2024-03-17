package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Membership;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

@Repository
public interface MembershipRepositoryCustom {
    Page<Membership> searchForMembership(Long clientId,
                                         Long membershipStateId,
                                         Long subscriptionId,
                                         String sort,
                                         String sortColumn,
                                         Integer pageNumber,
                                         Integer pageSize,
                                         Boolean status);
}
