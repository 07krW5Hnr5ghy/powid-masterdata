package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Membership;
import org.springframework.data.domain.Page;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.UUID;

@Repository
public interface MembershipRepositoryCustom {
    Page<Membership> searchForMembership(UUID clientId,
                                         UUID membershipStateId,
                                         UUID subscriptionId,
                                         OffsetDateTime registrationStartDate,
                                         OffsetDateTime registrationEndDate,
                                         OffsetDateTime updateStartDate,
                                         OffsetDateTime updateEndDate,
                                         String sort,
                                         String sortColumn,
                                         Integer pageNumber,
                                         Integer pageSize,
                                         Boolean status);
}
