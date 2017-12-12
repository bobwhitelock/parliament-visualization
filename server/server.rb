
require 'sinatra'
require 'sequel'

DB = Sequel.connect(ENV.fetch('DATABASE_URL'))

before do
  headers 'Access-Control-Allow-Origin' => '*'
end

get '/initial-data' do
  latest_vote = with_policy_ids(DB[:votes].order(:date).last)
  latest_vote_with_events = latest_vote.merge(
    voteEvents: vote_events_for(latest_vote),
  )

  # XXX Doing this like this slows this request from ~200ms to ~500ms on my
  # machine locally, due to needing a separate query to get the policy IDs for
  # every vote. Working out how to do this in a single query would probably be
  # a big improvement.
  votes = DB[:votes].map {|vote| with_policy_ids(vote)}.to_a

  {
    latestVote: latest_vote_with_events,
    votes: votes,
    policies: DB[:policies].to_a,
  }.to_json
end

get '/vote-events/:vote_id' do |vote_id|
  vote = DB[:votes][{id: vote_id}]
  vote_events_for(vote).to_json
end

def vote_events_for(vote)
  DB[:vote_events].where(
    vote_id: vote[:id]
  ).join(
    :people,
    person_id: :person_id,
    date: vote[:date]
  ).to_a
end

def with_policy_ids(vote)
  vote.merge(policyIds: policy_ids_for(vote))
end

def policy_ids_for(vote)
  DB[:votes_policies].where(vote_id: vote[:id]).map(:policy_id)
end
