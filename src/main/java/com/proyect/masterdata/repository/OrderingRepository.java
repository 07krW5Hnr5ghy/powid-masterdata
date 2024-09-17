package com.proyect.masterdata.repository;

import com.proyect.masterdata.domain.Ordering;
import com.proyect.masterdata.dto.DailySaleSummaryDTO;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import java.util.Date;
import java.util.List;

@Repository
public interface OrderingRepository extends JpaRepository<Ordering,Long> {
    List<Ordering> findAllByClientId(Long clientId);
    Ordering findByClientIdAndId(Long clientId,Long id);
    List<Ordering> findByClientIdAndRegistrationDateBetween(Long clientId,Date startDate,Date endDate);
    List<Ordering> findByClientIdAndRegistrationDateBetweenAndOrderStateId(Long clientId,Date startDate,Date endDate,Long orderStateId);
    @Query("SELECT FUNCTION('Date',o.registrationDate),COUNT(o) FROM Ordering o WHERE o.clientId = :clientId AND o.registrationDate BETWEEN :startDate AND :endDate GROUP BY FUNCTION('DATE', o.registrationDate) ORDER BY FUNCTION('DATE', o.registrationDate) ASC")
    List<Object[]> findAllOrdersByDate(
            @Param("clientId") Long clientId,
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate
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
            @Param("clientId") Long clientId,
            @Param("orderStateId") Long orderStateId,
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate
    );
    @Query("SELECT o.seller, COUNT(o) " +
            "FROM Ordering o " +
            "WHERE o.clientId = :clientId " +
            "AND o.registrationDate BETWEEN :startDate AND :endDate " +
            "GROUP BY o.seller " +
            "ORDER BY o.seller ASC")
    List<Object[]> findByClientIdAndRegistrationDateBetweenCountSeller(
            @Param("clientId") Long clientId,
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate
    );

    @Query("SELECT o.seller, d.name, p.name, dis.name, cc.name, COUNT(o) " +
            "FROM Ordering o " +
            "JOIN o.customer c " +
            "JOIN c.district dis " +
            "JOIN dis.province p " +
            "JOIN p.department d " +
            "JOIN o.closingChannel cc " +
            "WHERE o.clientId = :clientId " +
            "AND o.registrationDate BETWEEN :startDate AND :endDate " +
            "GROUP BY o.seller,d.name, p.name, dis.name, cc.name " +
            "ORDER BY o.seller ASC,d.name ASC, p.name ASC, dis.name ASC, cc.name ASC")
    List<Object[]> findByClientIdAndRegistrationDateBetweenCountReport(
            @Param("clientId") Long clientId,
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate
    );
}
