package com.proyect.masterdata.repository.impl;

import com.proyect.masterdata.domain.Audit;
import com.proyect.masterdata.repository.AuditRepositoryCustom;
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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Repository
public class AuditRepositoryCustomImpl implements AuditRepositoryCustom {
    @PersistenceContext(name = "entityManager")
    private EntityManager entityManager;
    @Override
    public Page<Audit> searchForAudit(Long userId, Long clientId, Long auditEventId, Date registrationStartDate, Date registrationEndDate, Date updateStartDate, Date updateEndDate, String sort, String sortColumn, Integer pageNumber, Integer pageSize) {
        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Audit> criteriaQuery = criteriaBuilder.createQuery(Audit.class);
        Root<Audit> itemRoot = criteriaQuery.from(Audit.class);

        criteriaQuery.select(itemRoot);

        List<Predicate> conditions = predicateConditions(userId, clientId, auditEventId,registrationStartDate, registrationEndDate, updateStartDate, updateEndDate, criteriaBuilder, itemRoot);

        if (!StringUtils.isBlank(sort) && !StringUtils.isBlank(sortColumn)) {

            List<Order> audiList = new ArrayList<>();

            if (sort.equalsIgnoreCase("ASC")) {
                audiList = listAsc(sortColumn, criteriaBuilder, itemRoot);
            }

            if (sort.equalsIgnoreCase("DESC")) {
                audiList = listDesc(sortColumn, criteriaBuilder, itemRoot);
            }

            criteriaQuery.where(conditions.toArray(new Predicate[] {})).orderBy(audiList);
        } else {
            criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        }

        TypedQuery<Audit> orderTypeQuery = entityManager.createQuery(criteriaQuery);
        orderTypeQuery.setFirstResult(pageNumber * pageSize);
        orderTypeQuery.setMaxResults(pageSize);

        Pageable pageable = PageRequest.of(pageNumber, pageSize);
        long count = getOrderCount(userId, clientId,auditEventId,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate);
        return new PageImpl<>(orderTypeQuery.getResultList(), pageable, count);
    }

    public List<Predicate> predicateConditions(
            Long userId,
            Long clientId,
            Long auditEventId,
            Date registrationStartDate,
            Date registrationEndDate,
            Date updateStartDate,
            Date updateEndDate,
            CriteriaBuilder criteriaBuilder,
            Root<Audit> itemRoot) {

        List<Predicate> conditions = new ArrayList<>();

        if(userId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("userId"),userId)));
        }

        if(clientId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("clientId"),userId)));
        }

        if(auditEventId != null){
            conditions.add(criteriaBuilder.and(criteriaBuilder.equal(itemRoot.get("auditEventId"),userId)));
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

        return conditions;
    }

    List<Order> listAsc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Audit> itemRoot) {

        List<Order> auditList = new ArrayList<>();

        if(sortColumn.equals("userId")){
            auditList.add(criteriaBuilder.asc(itemRoot.get("userId")));
        }

        if(sortColumn.equals("clientId")){
            auditList.add(criteriaBuilder.asc(itemRoot.get("clientId")));
        }

        if(sortColumn.equals("auditEventId")){
            auditList.add(criteriaBuilder.asc(itemRoot.get("auditEventId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            auditList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            auditList.add(criteriaBuilder.asc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            auditList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            auditList.add(criteriaBuilder.asc(itemRoot.get("updateDate")));
        }

        return auditList;
    }

    List<Order> listDesc(
            String sortColumn,
            CriteriaBuilder criteriaBuilder,
            Root<Audit> itemRoot) {
        List<Order> auditList = new ArrayList<>();

        if(sortColumn.equals("userId")){
            auditList.add(criteriaBuilder.desc(itemRoot.get("userId")));
        }

        if(sortColumn.equals("clientId")){
            auditList.add(criteriaBuilder.desc(itemRoot.get("clientId")));
        }

        if(sortColumn.equals("auditEventId")){
            auditList.add(criteriaBuilder.desc(itemRoot.get("auditEventId")));
        }

        if (sortColumn.equalsIgnoreCase("registrationStartDate")) {
            auditList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("registrationEndDate")) {
            auditList.add(criteriaBuilder.desc(itemRoot.get("registrationDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateStartDate")) {
            auditList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        if (sortColumn.equalsIgnoreCase("updateEndDate")) {
            auditList.add(criteriaBuilder.desc(itemRoot.get("updateDate")));
        }

        return auditList;
    }

    private long getOrderCount(Long userId,Long clientId, Long auditEventId,Date registrationStartDate,Date registrationEndDate,Date updateStartDate,Date updateEndDate) {

        CriteriaBuilder criteriaBuilder = entityManager.getCriteriaBuilder();
        CriteriaQuery<Long> criteriaQuery = criteriaBuilder.createQuery(Long.class);
        Root<Audit> itemRoot = criteriaQuery.from(Audit.class);

        criteriaQuery.select(criteriaBuilder.count(itemRoot));
        List<Predicate> conditions = predicateConditions(userId, clientId, auditEventId,registrationStartDate,registrationEndDate,updateStartDate,updateEndDate, criteriaBuilder, itemRoot);
        criteriaQuery.where(conditions.toArray(new Predicate[] {}));
        return entityManager.createQuery(criteriaQuery).getSingleResult();
    }
}
