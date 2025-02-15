package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.CancellationReason;
import com.proyect.masterdata.repository.CancellationReasonRepositoryCustom;
import io.micrometer.common.util.StringUtils;
import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.persistence.TypedQuery;
import jakarta.persistence.criteria.*;
import org.springframework.data.domain.Page;
import org.springframework.data.domain.PageImpl;
import org.springframework.data.domain.PageRequest;
import org.springframework.data.domain.Pageable;
import org.springframework.stereotype.Repository;

import java.time.OffsetDateTime;
import java.util.ArrayList;
import java.util.List;

@Repository
public class CancellationReasonRepositoryCustomImpl implements CancellationReasonRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<CancellationReason> searchForCancellationReason(
            String name, 
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate, 
            OffsetDateTime updateStartDate, 
            OffsetDateTime updateEndDate, 
            String sort, 
            String sortColumn, 
            Integer pageNumber, 
            Integer pageSize, 
            Boolean status) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<CancellationReason> criteriaQuery = criteriaBuilder.createQuery(CancellationReason.class);
        Root<CancellationReason> itemRoot = criteriaQuery.from(CancellationReason.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(name,registrationStartDate, registrationEndDate, updateStartDate, updateEndDate, status, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> cancellationReasonList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                cancellationReasonList = listAsc(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                cancellationReasonList = listDesc(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(cancellationReasonList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<CancellationReason> orderTypeQuery = entityManager.createQuery(criteriaQuery);
        orderTypeQuery.setFirstResult(pageNumber * pageSize);
        orderTypeQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(name,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate, status);
        return new PageImpl<>(orderTypeQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            String name,
            OffsetDateTime registrationStartDate,
            OffsetDateTime registrationEndDate,
            OffsetDateTime updateStartDate,
            OffsetDateTime updateEndDate,
            Boolean status,
            CriteriaBuilder criteriaBuilder,
            Root<CancellationReason> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if (name != null) {
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.equal(criteriaBuilder.upper(itemRoot.get("name")), name.toUpperCase())));
        }

        if(registrationStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("registrationDate"),registrationStartDate)
                    )
            );
        }

        if(registrationEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("registrationDate"),registrationEndDate)
                    )
            );
        }

        if(updateStartDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.greaterThanOrEqualTo(itemRoot.get("updateDate"),updateStartDate)
                    )
            );
        }

        if(updateEndDate!=null){
            conditions.add(
                    criteriaBuilder.and(
                            criteriaBuilder.lessThanOrEqualTo(itemRoot.get("updateDate"),updateEndDate)
                    )
            );
        }

        if (status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isTrue(itemRoot.get("status"))));
        }

        if (!status) {
            conditions.add(criteriaBuilder.and(criteriaBuilder.isFalse(itemRoot.get("status"))));
        }

        return conditions;
    }

    List<Order> listAsc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CancellationReason> itemRoot) {

        List<Order> cancellationReasonList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            cancellationReasonList.add(criteriaBuilder.asc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            cancellationReasonList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            cancellationReasonList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            cancellationReasonList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            cancellationReasonList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }
        
        return cancellationReasonList;
    }

    List<Order> listDesc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<CancellationReason> itemRoot) {
        List<Order> cancellationReasonList = new ArrayList<>();

        if (sortColumn.equalsIgnoreCase("NAME")) {
            cancellationReasonList.add(criteriaBuilder.desc(itemRoot.get("name")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            cancellationReasonList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            cancellationReasonList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            cancellationReasonList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            cancellationReasonList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return cancellationReasonList;
    }

    private long getOrderCount(String name,OffsetDateTime registrationStartDate,OffsetDateTime registrationEndDate,OffsetDateTime updateStartDate,OffsetDateTime updateEndDate, Boolean status) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<CancellationReason> itemRoot = criteriaQuery.from(CancellationReason.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(name,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate, status, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
