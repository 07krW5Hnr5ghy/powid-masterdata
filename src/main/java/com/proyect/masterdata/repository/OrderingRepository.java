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
    List<Ordering> findByClientIdAndUpdateDateBetween(Long clientId,Date startDate,Date endDate);
    List<Ordering> findByClientIdAndUpdateDateBetweenAndOrderStateId(Long clientId,Date startDate,Date endDate,Long orderStateId);
    List<Ordering> findByUpdateDateBetween(Date startDate, Date endDate);
    @Query("SELECT FUNCTION('Date',o.registrationDate),COUNT(o) FROM Ordering o WHERE o.clientId = :clientId AND o.registrationDate BETWEEN :startDate AND :endDate GROUP BY FUNCTION('DATE', o.registrationDate) ORDER BY FUNCTION('DATE', o.registrationDate) ASC")
    List<Object[]> findAllOrdersByDate(
            @Param("clientId") Long clientId,
            @Param("startDate") Date startDate,
            @Param("endDate") Date endDate
    );

}
