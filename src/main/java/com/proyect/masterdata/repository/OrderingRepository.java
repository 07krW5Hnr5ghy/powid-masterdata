package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.DailySaleSummaryDTO;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.Date;
import java.util.List;
import java.util.UUID;

@Repository
public interface OrderingRepository extends JpaRepository<Ordering, UUID> {
    List<Ordering> findAllByClientId(UUID clientId);
    Ordering findByClientIdAndId(UUID clientId,UUID id);
    List<Ordering> findByClientIdAndRegistrationDateBetween(UUID clientId, OffsetDateTime startDate, OffsetDateTime endDate);
    List<Ordering> findByClientIdAndRegistrationDateBetweenAndOrderStateId(UUID clientId,OffsetDateTime startDate,OffsetDateTime endDate,UUID orderStateId);
    @Query("SELECT FUNCTION('Date',o.registrationDate),COUNT(o) FROM Ordering o WHERE o.clientId = :clientId AND o.registrationDate BETWEEN :startDate AND :endDate GROUP BY FUNCTION('DATE', o.registrationDate) ORDER BY FUNCTION('DATE', o.registrationDate) ASC")
    List<Object[]> findAllOrdersByDate(
            @Param("clientId") UUID clientId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );
    @Query("SELECT FUNCTION('DATE', o.registrationDate), COUNT(o) " +
            "FROM Ordering o " +
            "JOIN o.orderState s " +
            "WHERE o.clientId = :clientId " +
            "AND s.id = :orderStateId " +
            "AND o.registrationDate BETWEEN :startDate AND :endDate " +
            "GROUP BY FUNCTION('DATE', o.registrationDate) " +
            "ORDER BY FUNCTION('DATE', o.registrationDate) ASC")
    List<Object[]> findOrderCountByDateAndStatus(
            @Param("clientId") UUID clientId,
            @Param("orderStateId") UUID orderStateId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );
    @Query("SELECT o.seller, COUNT(o) " +
            "FROM Ordering o " +
            "WHERE o.clientId = :clientId " +
            "AND o.registrationDate BETWEEN :startDate AND :endDate " +
            "GROUP BY o.seller " +
            "ORDER BY o.seller ASC")
    List<Object[]> findByClientIdAndRegistrationDateBetweenCountSeller(
            @Param("clientId") UUID clientId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );
    @Query("SELECT o.seller,d.name, p.name, dis.name, cs.name, COUNT(o) " +
            "FROM Ordering o " +
            "JOIN o.customer c " +
            "JOIN c.district dis " +
            "JOIN dis.province p " +
            "JOIN p.department d " +
            "JOIN o.closingChannel cs " +
            "WHERE o.clientId = :clientId " +
            "AND o.registrationDate BETWEEN :startDate AND :endDate " +
            "GROUP BY o.seller, d.name, p.name, dis.name, cs.name " +
            "ORDER BY o.seller, d.name, p.name, dis.name, cs.name ASC")
    List<Object[]> findByClientIdAndRegistrationDateBetweenCountSellerReport(
            @Param("clientId") UUID clientId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );
    @Query("SELECT o.orderState.name AS stateName, COUNT(o) AS count " +
            "FROM Ordering o " +
            "WHERE o.clientId = :clientId " +
            "AND o.registrationDate BETWEEN :startDate AND :endDate " +
            "GROUP BY o.orderState.name " +
            "ORDER BY o.orderState.name ASC")
    List<Object[]> findByClientIdAndStateRegistrationDateBetween(
            @Param("clientId") UUID clientId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );

    @Query("SELECT o.saleChannel.name AS saleChannelName, COUNT(o) AS count " +
            "FROM Ordering o " +
            "WHERE o.clientId = :clientId " +
            "AND o.registrationDate BETWEEN :startDate AND :endDate " +
            "GROUP BY o.saleChannel.name " +
            "ORDER BY o.saleChannel.name ASC")
    List<Object[]> findByClientIdAndSaleChannelRegistrationDateBetween(
            @Param("clientId") UUID clientId,
            @Param("startDate") OffsetDateTime startDate,
            @Param("endDate") OffsetDateTime endDate
    );

    Long countByClientId(UUID clientId);
}
