package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.OrderContacted;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import java.time.OffsetDateTime;
import java.util.UUID;

@Repository
public interface OrderContactedRepository extends JpaRepository<OrderContacted, UUID> {
    OrderContacted findByOrderId(UUID orderId);

    @Modifying
    @Transactional
    @Query("UPDATE OrderContacted o SET " +
            "o.userId = :userId, " +
            "o.agentId = :agentId, " +
            "o.updateDate = :updateDate," +
            "o.observations = :observations " +
            "WHERE o.orderId = :orderId")
    void selectAgentOrderContact(
            @Param("orderId") UUID orderId,
            @Param("userId") UUID userId,
            @Param("agentId") UUID agentId,
            @Param("updateDate") OffsetDateTime updateDate,
            @Param("observations") String observations
    );

    @Modifying
    @Transactional
    @Query("UPDATE OrderContacted  o SET " +
            "o.contacted = :contacted, " +
            "o.updateDate =:updateDate, " +
            "o.userId = :userId, " +
            "o.clientId = :clientId, " +
            "o.observations = :observations " +
            "WHERE o.orderId = :orderId")
    void markContacted(
            @Param("contacted") boolean contacted,
            @Param("updateDate") OffsetDateTime updateDate,
            @Param("userId") UUID userId,
            @Param("clientId") UUID clientId,
            @Param("observations") String observations,
            @Param("orderId") UUID orderId
    );

    @Modifying
    @Transactional
    @Query("UPDATE OrderContacted o SET " +
            "o.updateDate = :updateDate, " +
            "o.userId = :userId, " +
            "o.observations = :observations " +
            "WHERE o.orderId = :orderId")
    void selectCourierQuery(
            @Param("updateDate") OffsetDateTime updateDate,
            @Param("userId") UUID userId,
            @Param("observations") String observations,
            @Param("orderId") UUID orderId
    );
}
